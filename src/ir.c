#include "ir.h"

#include <stdint.h>

#include "ast.h"
#include "str_pool.h"
#include "sym_table.h"
#include "vec.h"
#include "consteval.h"

Vec_Impl(Instr);
Vec_Impl(Func);
Vec_Impl(Global);

void ir_new_func(IrGen *ir_gen, StrIdx name, uint32_t arity) {
    Func new_func = { .instrs = { 0 }, .name = name, .locals = 0, .arity = arity };
    FuncVec_push(&ir_gen->ir.funcs, new_func);
    ir_gen->cur_func = &ir_gen->ir.funcs.elems[ir_gen->ir.funcs.len - 1];
}

IrLoc ir_mk_loc(IrGen *ir_gen) {
    ir_gen->vstack_top += 1;
    if (ir_gen->cur_func->locals < ir_gen->vstack_top) {
        ir_gen->cur_func->locals = ir_gen->vstack_top;
    }
    return ir_gen->vstack_top;
}

void ir_free_loc(IrGen *ir_gen, IrLoc v) {
    assert(v == ir_gen->vstack_top);
    ir_gen->vstack_top--;
}

uint32_t ir_mk_label(IrGen *ir_gen) {
    return ir_gen->label_gen++;
}

static inline void ir_emit(IrGen *ir_gen, Instr instr) {
    InstrVec_push(&ir_gen->cur_func->instrs, instr);
}

typedef struct IrExpr {
    enum { Ir_Tmp, Ir_Var, Ir_Lit } kind;
    int32_t val;
} IrExpr;

void ir_materialize_expr(IrGen *ir_gen, IrExpr *expr, IrLoc where) {
    switch (expr->kind) {
    case Ir_Tmp: {
        if (where != 0) {
            ir_emit(
                ir_gen, (Instr){ .op = Op_MOV, .dst = where, .a = expr->val });
            ir_free_loc(ir_gen, expr->val);
            expr->kind = Ir_Var;
            expr->val = where;
        }
    } break;

    case Ir_Var: {
        if (where != 0 && where != expr->val) {
            ir_emit(
                ir_gen, (Instr){ .op = Op_MOV, .dst = where, .a = expr->val });
            expr->val = where;
        }
    } break;

    case Ir_Lit: {
        if (where == 0) {
            where = ir_mk_loc(ir_gen);
            expr->kind = Ir_Tmp;
        }
        ir_emit(
            ir_gen, (Instr){ .op = Op_MOV_LIT, .dst = where, .a = expr->val });
            expr->val = where;
    } break;
    }
}

static bool ir_use_expr(IrGen *ir_gen, IrExpr *expr) {
    if (expr->kind == Ir_Tmp) {
        ir_free_loc(ir_gen, expr->val);
    }
    return expr->kind == Ir_Lit;
}

Vec_Proto(IrExpr);
Vec_Impl(IrExpr);

////////////////////////////////////////////////////////////////////////////////

static IrExpr ir_gen_expr(IrGen *ir_gen, Ast ast, NodeIdx idx, IrLoc can_dst);

static void ir_gen_meth_call(IrGen *ir_gen, Ast ast, NodeIdx idx, IrLoc dst) {
    AstNodeFull_MethCall meth_call = Ast_full_meth_call(ast, idx);
    uint32_t argc = meth_call.args_end - meth_call.args_begin;

    // compile arguments into vector
    IrExprVec args = { 0 };
    for (uint32_t i = meth_call.args_begin; i < meth_call.args_end; i++) {
        IrExpr arg = ir_gen_expr(ir_gen, ast, i, 0);
        IrExprVec_push(&args, arg);
    }

    // emit and free arguments
    for (int32_t i = argc - 1; i >= 0; i--) {
        Op op = ir_use_expr(ir_gen, &args.elems[i]) ? Op_ARG_LIT : Op_ARG;
        ir_emit(ir_gen, (Instr){ .op = op, .a = args.elems[i].val, .dst = i });
    }

    IrExprVec_free(&args);

    // emit call into requested location
    ir_emit(
        ir_gen,
        (Instr){ .op = Op_CALL,
                 .a = meth_call.meth_ident,
                 .b = argc,
                 .dst = dst });
}

static IrExpr ir_gen_expr(IrGen *ir_gen, Ast ast, NodeIdx idx, IrLoc can_dst) {
    AstNode *node = &ast.nodes[idx];

    switch (node->kind) {
    case AstNodeKind_METH_CALL: {
        IrLoc dst = can_dst == 0 ? ir_mk_loc(ir_gen) : can_dst;
        ir_gen_meth_call(ir_gen, ast, idx, dst);

        return (IrExpr){ .kind = can_dst == 0 ? Ir_Tmp : Ir_Var, .val = dst };
    }

    case AstNodeKind_VAR: {
        StrIdx ident = node->data.lhs;
        SymInfo *sym_info = symtable_get_symbol(&ir_gen->sym_table, ident);

        if (sym_info == NULL) { // global
            IrLoc dst = can_dst == 0 ? ir_mk_loc(ir_gen) : can_dst;
            ir_emit(ir_gen, (Instr){ .op = Op_GET_GLOBAL, .a = ident, .dst = dst });

            return (IrExpr){ .kind = can_dst == 0 ? Ir_Tmp : Ir_Var, .val = dst };
        } else { // local
            return (IrExpr){ .kind = Ir_Var, .val = sym_info->ir_info.loc };
        }
    }

    case AstNodeKind_INT_LIT:
    case AstNodeKind_BOOL_LIT: 
        return (IrExpr){ .kind = Ir_Lit, .val = node->data.lhs };

    case AstNodeKind_UNM:
    case AstNodeKind_NEG: {
        IrExpr operand = ir_gen_expr(ir_gen, ast, node->data.lhs, can_dst);

        if (operand.kind == Ir_Lit) { // fold
            switch (node->kind) {
            case AstNodeKind_UNM: operand.val = -operand.val; break;
            case AstNodeKind_NEG: operand.val = !operand.val; break;
            default: unreachable;
            }

            return operand;
        } else { // emit
            assert(!ir_use_expr(ir_gen, &operand));

            IrLoc dst = can_dst == 0 ? ir_mk_loc(ir_gen) : can_dst;
            ir_emit(
                ir_gen,
                (Instr){ // HACK: this is awesome.
                        .op = node->kind - (AstNodeKind_UNM - Op_UNM),
                        .a = operand.val,
                        .dst = dst });

            return (IrExpr){ .kind = can_dst == 0 ? Ir_Tmp : Ir_Var, .val = dst };
        }
    }

    case AstNodeKind_MUL:
    case AstNodeKind_DIV:
    case AstNodeKind_MOD:
    case AstNodeKind_ADD:
    case AstNodeKind_SUB:
    case AstNodeKind_LT:
    case AstNodeKind_GT:
    case AstNodeKind_EQ: {
        IrExpr lhs = ir_gen_expr(ir_gen, ast, node->data.lhs, 0);
        IrExpr rhs = ir_gen_expr(ir_gen, ast, node->data.rhs, 0);

        if (lhs.kind == Ir_Lit && rhs.kind == Ir_Lit) { // fold
            switch (node->kind) {
            case AstNodeKind_MUL: lhs.val *= rhs.val; break;
            case AstNodeKind_DIV: lhs.val /= rhs.val; break;
            case AstNodeKind_MOD: lhs.val %= rhs.val; break;
            case AstNodeKind_ADD: lhs.val += rhs.val; break;
            case AstNodeKind_SUB: lhs.val -= rhs.val; break;
            case AstNodeKind_LT: lhs.val = lhs.val < rhs.val; break;
            case AstNodeKind_GT: lhs.val = lhs.val > rhs.val; break;
            case AstNodeKind_EQ: lhs.val = lhs.val == rhs.val; break;
            default: unreachable;
            }

            return lhs;
        } else { // emit
            // swap if lhs is lit and op commutes
            if (lhs.kind == Ir_Lit &&
                (node->kind == AstNodeKind_MUL || node->kind == AstNodeKind_ADD)) {
                    IrExpr tmp = lhs;
                    lhs = rhs;
                    rhs = tmp;
            }

            ir_materialize_expr(ir_gen, &lhs, 0);
            bool use_lit_op = ir_use_expr(ir_gen, &rhs);

            assert(!ir_use_expr(ir_gen, &lhs));

            IrLoc dst = can_dst == 0 ? ir_mk_loc(ir_gen) : can_dst;
            Op op = node->kind - (AstNodeKind_MUL - Op_MUL);
            op += use_lit_op ? Op_MUL_LIT - Op_MUL : 0;

            ir_emit(
                ir_gen, (Instr){ .op = op, .a = lhs.val, .b = rhs.val, .dst = dst});

            return (IrExpr){ .kind = can_dst == 0 ? Ir_Tmp : Ir_Var, .val = dst };
        }
    }

    case AstNodeKind_AND:
    case AstNodeKind_OR: {
        uint32_t label = ir_mk_label(ir_gen);
        IrLoc tmp = ir_mk_loc(ir_gen);

        IrExpr lhs = ir_gen_expr(ir_gen, ast, node->data.lhs, tmp);
        ir_materialize_expr(ir_gen, &lhs, tmp);

        ir_emit(
            ir_gen,
            (Instr){
                .op = node->kind == AstNodeKind_AND ? Op_JMP_IF_F : Op_JMP_IF_T,
                .a = lhs.val,
                .b = label,
            });

        IrExpr rhs = ir_gen_expr(ir_gen, ast, node->data.rhs, tmp);
        ir_materialize_expr(ir_gen, &rhs, tmp);

        ir_emit(ir_gen, (Instr){ .op = Op_LABEL, .a = label });

        if (can_dst == 0) {
            return (IrExpr){ .kind = Ir_Tmp, .val = tmp };
        } else {
            ir_use_expr(ir_gen, &rhs);
            ir_emit(ir_gen, (Instr){ .op = Op_MOV, .dst = can_dst, .a = tmp });
            return (IrExpr){ .kind = Ir_Var, .val = tmp };
        }
    }

    default:
        unreachable;
    }
}

static bool ir_gen_stmt(IrGen *ir_gen, Ast ast, NodeIdx idx) {
    AstNode *node = &ast.nodes[idx];

    switch (node->kind) {
    case AstNodeKind_BLOCK: {
        AstNodeFull_List block = Ast_full_block(ast, idx);
        bool has_ret = false;

        symtable_push_scope(&ir_gen->sym_table);
        for (NodeIdx i = block.begin; i < block.end && !has_ret; i++) {
            has_ret = has_ret || ir_gen_stmt(ir_gen, ast, i);
        }
        symtable_pop_scope(&ir_gen->sym_table, &ir_gen->vstack_top);

        return has_ret;
    }

    case AstNodeKind_VAR_DECL_INIT: {
        AstNodeFull_VarDecl var_decl = Ast_full_var_decl(ast, idx);

        IrLoc where = ir_mk_loc(ir_gen);
        IrExpr expr = ir_gen_expr(ir_gen, ast, var_decl.init_expr, where);
        ir_materialize_expr(ir_gen, &expr, where);

        symtable_put_symbol(
            &ir_gen->sym_table,
            var_decl.ident,
            (TypeInfo){ 0 },
            (IrInfo){ .loc = where });

        return false;
    }

    case AstNodeKind_ASGN: {
        AstNodeFull_Asgn asgn = Ast_full_asgn(ast, idx);
        SymInfo *sym_info =
            symtable_get_symbol(&ir_gen->sym_table, asgn.target);

        IrLoc where =
            sym_info == NULL ? ir_mk_loc(ir_gen) : sym_info->ir_info.loc;
        IrExpr expr = ir_gen_expr(ir_gen, ast, asgn.expr, where);
        ir_materialize_expr(ir_gen, &expr, where);

        if (sym_info == NULL) { // global
            ir_use_expr(ir_gen, &expr);
            ir_emit(
                ir_gen,
                (Instr){ .op = Op_SET_GLOBAL, .a = where, .dst = asgn.target });
        }

        return false;
    }

    case AstNodeKind_IF_SMP:
    case AstNodeKind_IF_ALT: {
        AstNodeFull_If if_node = Ast_full_if(ast, idx);

        uint32_t l_end = ir_mk_label(ir_gen);
        uint32_t l_else = ir_mk_label(ir_gen);

        IrExpr test = ir_gen_expr(ir_gen, ast, if_node.cond, 0);
        ir_materialize_expr(ir_gen, &test, 0);

        ir_use_expr(ir_gen, &test);
        ir_emit(ir_gen, (Instr){ .op = Op_JMP_IF_F, .a = test.val, .b = l_else });

        bool then_has_ret = ir_gen_stmt(ir_gen, ast, if_node.then_b);
        bool else_has_ret = false;

        if (if_node.else_b != NO_NODE) {
            if (!then_has_ret) {
                ir_emit(ir_gen, (Instr){ .op = Op_JMP, .a = l_end });
            }

            ir_emit(ir_gen, (Instr){ .op = Op_LABEL, .a = l_else });

            else_has_ret = ir_gen_stmt(ir_gen, ast, if_node.else_b);

            if (!then_has_ret) {
                ir_emit(ir_gen, (Instr){ .op = Op_LABEL, .a = l_end });
            }
        } else { // emit l_else, just to skip then_b
            ir_emit(ir_gen, (Instr){ .op = Op_LABEL, .a = l_else });
        }

        return then_has_ret && else_has_ret;
    }

    case AstNodeKind_WHILE: {
        AstNodeFull_While while_node = Ast_full_while(ast, idx);

        uint32_t l_test = ir_mk_label(ir_gen);
        ir_emit(ir_gen, (Instr){ .op = Op_LABEL, .a = l_test });

        IrExpr test = ir_gen_expr(ir_gen, ast, while_node.cond, 0);
        ir_materialize_expr(ir_gen, &test, 0);

        uint32_t l_escape = ir_mk_label(ir_gen);

        ir_use_expr(ir_gen, &test);
        ir_emit(ir_gen, (Instr){ .op = Op_JMP_IF_F, .a = test.val, .b = l_escape });

        bool block_has_ret = ir_gen_stmt(ir_gen, ast, while_node.body);

        if (!block_has_ret) {
            ir_emit(ir_gen, (Instr){ .op = Op_JMP, .a = l_test });
        }

        ir_emit(ir_gen, (Instr){ .op = Op_LABEL, .a = l_escape });

        return false;
    }

    case AstNodeKind_RET: {
        NodeIdx expr_idx = node->data.lhs;

        IrExpr ret_val = (IrExpr){ .kind = Ir_Lit, .val = 0 };
        if (expr_idx != NO_NODE) {
            ret_val = ir_gen_expr(ir_gen, ast, expr_idx, 0);
        }

        if (ir_use_expr(ir_gen, &ret_val)) {
            ir_emit(ir_gen, (Instr){ .op = Op_RET_LIT, .a = ret_val.val });
        } else {
            ir_emit(ir_gen, (Instr){ .op = Op_RET, .a = ret_val.val });
        }

        return true;
    }

    case AstNodeKind_METH_CALL: {
        ir_gen_meth_call(ir_gen, ast, idx, 0);
        return false;
    }

    default:
        unreachable;
    }
}

static void ir_gen_meth_body(IrGen *ir_gen, Ast ast, NodeIdx idx) {
    bool has_ret = ir_gen_stmt(ir_gen, ast, idx);

    if (!has_ret) {
        ir_emit(ir_gen, (Instr){ .op = Op_RET_LIT, .a = 0 });
    }
}

static void ir_gen_meth_decl(IrGen *ir_gen, Ast ast, NodeIdx idx) {
    AstNodeFull_MethDecl meth_decl = Ast_full_meth_decl(ast, idx);

    if (meth_decl.body == NO_NODE) {
        return;
    }

    AstNodeFull_List params = Ast_full_list(ast, meth_decl.params);

    ir_new_func(ir_gen, meth_decl.ident, params.end - params.begin);

    symtable_push_scope(&ir_gen->sym_table); // parameter scope

    for (NodeIdx i = params.begin; i < params.end; i++) {
        AstNode *param_node = &ast.nodes[i];
        assert(param_node->kind == AstNodeKind_PARAM);
        Type param_type = param_node->data.lhs;
        Type param_name = param_node->data.rhs;

        IrLoc param_var = ir_mk_loc(ir_gen);
        symtable_put_symbol(
            &ir_gen->sym_table,
            param_name,
            (TypeInfo){ .base = param_type },
            (IrInfo){ .loc = param_var });
    }

    ir_gen_meth_body(ir_gen, ast, meth_decl.body);

    symtable_pop_scope(
        &ir_gen->sym_table, &ir_gen->vstack_top); // parameter scope
}

static void ir_gen_global(IrGen *ir_gen, Ast ast, NodeIdx i) {
    GlobalVec *globals = &ir_gen->ir.globals;
    AstNodeFull_VarDecl var_init = Ast_full_var_decl(ast, i);

    int32_t eval_result = consteval(ast, var_init.init_expr, globals);
    Global data = {
        .name = var_init.ident,
        .value = eval_result,
    };

    GlobalVec_push(globals, data);
}

static void ir_gen_prog(IrGen *ir_gen, Ast ast) {
    AstNodeFull_List prog = Ast_full_prog(ast);

    for (uint32_t i = prog.begin; i < prog.end; i++) {
        AstNode *node = &ast.nodes[i];
        switch (node->kind) {
        case AstNodeKind_VAR_DECL_INIT:
            ir_gen_global(ir_gen, ast, i);
            break;

        case AstNodeKind_METH_DECL_IMPL:
            ir_gen_meth_decl(ir_gen, ast, i);
            break;

        case AstNodeKind_METH_DECL:
            break; // don't do anything for extern declarations
        default:
            unreachable;
        }
    }
}

Ir mk_ir(const Ast ast) {
    IrGen ir_gen = { 0 };

    ir_gen_prog(&ir_gen, ast);

    SymInfoVec_free(&ir_gen.sym_table.symbols);

    return ir_gen.ir;
}
