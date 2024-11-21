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

/************************************ EMIT ************************************/

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

IrLbl ir_mk_lbl(IrGen *ir_gen) {
    return ir_gen->label_gen++;
}

static inline void ir_emit(IrGen *ir_gen, Instr instr) {
    InstrVec_push(&ir_gen->cur_func->instrs, instr);
}

/************************************ EXPR ************************************/

typedef struct IrExpr {
    enum { Ir_Tmp, Ir_Var, Ir_Lit } kind;
    int32_t val;
} IrExpr;

static IrExpr ir_expr_new_with_loc(IrGen *ir_gen, IrLoc where) {
    if (where != NEW_LOC) {
        return (IrExpr){ .kind = Ir_Var, .val = where };
    } else {
        return (IrExpr){ .kind = Ir_Tmp, .val = ir_mk_loc(ir_gen) };
    }
}

static void ir_expr_materialize(IrGen *ir_gen, IrExpr *expr, IrLoc where) {
    switch (expr->kind) {
    case Ir_Tmp: {
        if (where != NEW_LOC) {
            ir_emit(
                ir_gen, (Instr){ .op = Op_MOV, .dst = where, .a = expr->val });
            ir_free_loc(ir_gen, expr->val);
            expr->kind = Ir_Var;
            expr->val = where;
        }
    } break;

    case Ir_Var: {
        if (where != NEW_LOC && where != expr->val) {
            ir_emit(
                ir_gen, (Instr){ .op = Op_MOV, .dst = where, .a = expr->val });
            expr->val = where;
        }
    } break;

    case Ir_Lit: {
        if (where == NEW_LOC) {
            where = ir_mk_loc(ir_gen);
            expr->kind = Ir_Tmp;
        }
        ir_emit(
            ir_gen, (Instr){ .op = Op_MOV_LIT, .dst = where, .a = expr->val });
            expr->val = where;
    } break;
    }
}

static bool ir_expr_use(IrGen *ir_gen, IrExpr *expr) {
    if (expr->kind == Ir_Tmp) {
        ir_free_loc(ir_gen, expr->val);
    }
    return expr->kind == Ir_Lit;
}

Vec_Proto(IrExpr);
Vec_Impl(IrExpr);

/************************************ TEST ************************************/

static JmpCond ir_cond_swap(JmpCond cond) {
    switch (cond) {
    case JmpCond_LT: return JmpCond_GE;
    case JmpCond_GT: return JmpCond_LE;
    case JmpCond_EQ: return JmpCond_EQ;
    case JmpCond_GE: return JmpCond_LT;
    case JmpCond_LE: return JmpCond_GT;
    case JmpCond_NE: return JmpCond_NE;
        default: unreachable;
    }
}

static JmpCond ir_cond_invert(JmpCond cond) {
    switch (cond) {
    case JmpCond_LT: return JmpCond_GE;
    case JmpCond_GT: return JmpCond_LE;
    case JmpCond_EQ: return JmpCond_NE;
    case JmpCond_GE: return JmpCond_LT;
    case JmpCond_LE: return JmpCond_GT;
    case JmpCond_NE: return JmpCond_EQ;
    default: unreachable;
    }
}

typedef enum {
    IrTest_False = 0b01,
    IrTest_True  = 0b10,
    IrTest_Both  = 0b11,
} IrTest;

#define TEST_HAS(test, gen) (((test) & IrTest_##gen) != 0)

typedef struct { IrLbl t, f, n; } IrCtrl;

static void ir_branch_materialize(IrGen *ir_gen, JmpCond cond, IrCtrl ctrl) {
    if (ctrl.f == ctrl.n) {
        ir_emit(ir_gen, (Instr){ .op = Op_JMP_IF, .a = cond, .b = ctrl.t });
    } else {
        cond = ir_cond_invert(cond);
        ir_emit(ir_gen, (Instr){ .op = Op_JMP_IF, .a = cond, .b = ctrl.f });
        if (ctrl.t != ctrl.n) {
            ir_emit(ir_gen, (Instr){ .op = Op_JMP, .a = ctrl.t });
        }
    }
}

/************************************ FOLD ************************************/

static IrTest ir_fold_test(AstNodeKind op, IrExpr lhs, IrExpr rhs) {
    switch (op) {
    case AstNodeKind_LT: return lhs.val < rhs.val ? IrTest_True : IrTest_False;
    case AstNodeKind_GT: return lhs.val > rhs.val ? IrTest_True : IrTest_False;
    case AstNodeKind_EQ: return lhs.val == rhs.val ? IrTest_True : IrTest_False;
    default: unreachable;
    }
}

static IrExpr ir_fold_expr_un(AstNodeKind op, IrExpr arg) {
    switch (op) {
    case AstNodeKind_UNM: return (IrExpr){ Ir_Lit, -arg.val };
    case AstNodeKind_NEG: return (IrExpr){ Ir_Lit, !arg.val };
    default: unreachable;
    }
}

static IrExpr ir_fold_expr_bin(AstNodeKind op, IrExpr lhs, IrExpr rhs) {
    switch (op) {
    case AstNodeKind_MUL: return (IrExpr){ Ir_Lit, lhs.val * rhs.val };
    case AstNodeKind_DIV: return (IrExpr){ Ir_Lit, lhs.val / rhs.val };
    case AstNodeKind_MOD: return (IrExpr){ Ir_Lit, lhs.val % rhs.val };
    case AstNodeKind_ADD: return (IrExpr){ Ir_Lit, lhs.val + rhs.val };
    case AstNodeKind_SUB: return (IrExpr){ Ir_Lit, lhs.val - rhs.val };
    default: unreachable;
    }
}

////////////////////////////////////////////////////////////////////////////////

static IrExpr ir_gen_expr(IrGen *ir_gen, Ast ast, NodeIdx idx, IrLoc can_dst);

static IrTest ir_gen_test(IrGen *ir_gen, Ast ast, NodeIdx idx, IrCtrl ctrl) {
    AstNode *node = &ast.nodes[idx];

    switch (node->kind) {
    case AstNodeKind_METH_CALL:
    case AstNodeKind_VAR: {
        IrExpr test = ir_gen_expr(ir_gen, ast, idx, NEW_LOC);
        assert(!ir_expr_use(ir_gen, &test));

        ir_emit(ir_gen, (Instr){ .op = Op_CMP_LIT, .a = test.val, .b = 0 });
        ir_branch_materialize(ir_gen, JmpCond_NE, ctrl);

        return IrTest_Both;
    }

    case AstNodeKind_BOOL_LIT:
        return node->data.lhs == 0 ? IrTest_False : IrTest_True;

    case AstNodeKind_NEG: {
        IrTest inverted = ir_gen_test(
            ir_gen, ast, node->data.lhs,
            (IrCtrl){ .t = ctrl.f, .f = ctrl.t, .n = ctrl.n});

        if (!TEST_HAS(inverted, Both)) {
            inverted ^= IrTest_Both;
        }
        return inverted;
    }

    case AstNodeKind_LT:
    case AstNodeKind_GT: 
    case AstNodeKind_EQ: {
        IrExpr lhs = ir_gen_expr(ir_gen, ast, node->data.lhs, NEW_LOC);
        IrExpr rhs = ir_gen_expr(ir_gen, ast, node->data.rhs, NEW_LOC);

        if (lhs.kind == Ir_Lit && rhs.kind == Ir_Lit) {
            return ir_fold_test(node->kind, lhs, rhs);
        }

        JmpCond cond = node->kind - (AstNodeKind_LT - JmpCond_LT);

        if (lhs.kind == Ir_Lit) { // swap operands and reverse cond
            IrExpr tmp = lhs;
            lhs = rhs;
            rhs = tmp;

            cond = ir_cond_swap(cond);
        }

        ir_expr_materialize(ir_gen, &lhs, NEW_LOC);
        bool use_lit_op = ir_expr_use(ir_gen, &rhs);

        assert(!ir_expr_use(ir_gen, &lhs));

        Op op = use_lit_op ? Op_CMP_LIT : Op_CMP;
        ir_emit(ir_gen, (Instr){ .op = op, .a = lhs.val, .b = rhs.val });
        ir_branch_materialize(ir_gen, cond, ctrl);

        return IrTest_Both;
    }

    case AstNodeKind_AND: {
        IrLbl lbl = ir_mk_lbl(ir_gen);

        IrTest lhs =
            ir_gen_test(
                ir_gen, ast, node->data.lhs,
                (IrCtrl){ .t = lbl, .f = ctrl.f, .n = lbl});

        if (lhs == IrTest_False) { return IrTest_False; }

        ir_emit(ir_gen, (Instr){ .op = Op_LBL, .a = lbl });

        IrTest rhs = ir_gen_test(ir_gen, ast, node->data.lhs, ctrl);

        if (lhs == IrTest_True) {
            return rhs;
        } else {
            return rhs | IrTest_False;
        }
    }

    case AstNodeKind_OR: {
        IrLbl lbl = ir_mk_lbl(ir_gen);

        IrTest lhs =
            ir_gen_test(
                ir_gen, ast, node->data.lhs,
                (IrCtrl){ .t = ctrl.t, .f = lbl, .n = lbl});

        if (lhs == IrTest_True) { return IrTest_True; }

        ir_emit(ir_gen, (Instr){ .op = Op_LBL, .a = lbl });

        IrTest rhs = ir_gen_test(ir_gen, ast, node->data.lhs, ctrl);

        if (lhs == IrTest_False) {
            return rhs;
        } else {
            return rhs | IrTest_True;
        }
    }

    default:
        unreachable;
    }
}

static void ir_gen_meth_call(IrGen *ir_gen, Ast ast, NodeIdx idx, IrLoc dst) {
    AstNodeFull_MethCall meth_call = Ast_full_meth_call(ast, idx);
    uint32_t argc = meth_call.args_end - meth_call.args_begin;

    // compile arguments into vector
    IrExprVec args = { 0 };
    for (uint32_t i = meth_call.args_begin; i < meth_call.args_end; i++) {
        IrExpr arg = ir_gen_expr(ir_gen, ast, i, NEW_LOC);
        IrExprVec_push(&args, arg);
    }

    // emit and free arguments
    for (int32_t i = argc - 1; i >= 0; i--) {
        Op op = ir_expr_use(ir_gen, &args.elems[i]) ? Op_ARG_LIT : Op_ARG;
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

static IrExpr ir_gen_expr(IrGen *ir_gen, Ast ast, NodeIdx idx, IrLoc where) {
    AstNode *node = &ast.nodes[idx];

    switch (node->kind) {
    case AstNodeKind_METH_CALL: {
        IrExpr dst = ir_expr_new_with_loc(ir_gen, where);
        ir_gen_meth_call(ir_gen, ast, idx, dst.val);

        return dst;
    }

    case AstNodeKind_VAR: {
        StrIdx ident = node->data.lhs;
        SymInfo *sym_info = symtable_get_symbol(&ir_gen->sym_table, ident);

        if (sym_info == NULL) { // global
            IrExpr res = ir_expr_new_with_loc(ir_gen, where);
            ir_emit(ir_gen, (Instr){ .op = Op_GET_GLOBAL, .a = ident, .dst = res.val });

            return res;
        } else { // local
            return (IrExpr){ .kind = Ir_Var, .val = sym_info->ir_info.loc };
        }
    }

    case AstNodeKind_INT_LIT:
    case AstNodeKind_BOOL_LIT: 
        return (IrExpr){ .kind = Ir_Lit, .val = node->data.lhs };

    case AstNodeKind_UNM:
    case AstNodeKind_NEG: {
        IrExpr operand = ir_gen_expr(ir_gen, ast, node->data.lhs, where);

        if (operand.kind == Ir_Lit) {
            return ir_fold_expr_un(node->kind, operand);
        }

        assert(!ir_expr_use(ir_gen, &operand));

        IrExpr res = ir_expr_new_with_loc(ir_gen, where);
        ir_emit(
            ir_gen,
            (Instr){ // HACK: this is awesome.
                    .op = node->kind - (AstNodeKind_UNM - Op_UNM),
                    .a = operand.val,
                    .dst = res.val });

        return res;
    }

    case AstNodeKind_MUL:
    case AstNodeKind_DIV:
    case AstNodeKind_MOD:
    case AstNodeKind_ADD:
    case AstNodeKind_SUB: {
        IrExpr lhs = ir_gen_expr(ir_gen, ast, node->data.lhs, NEW_LOC);
        IrExpr rhs = ir_gen_expr(ir_gen, ast, node->data.rhs, NEW_LOC);

        if (lhs.kind == Ir_Lit && rhs.kind == Ir_Lit) {
            return ir_fold_expr_bin(node->kind, lhs, rhs);
        }

        // swap if lhs is lit and op commutes
        if (lhs.kind == Ir_Lit &&
            (node->kind == AstNodeKind_MUL || node->kind == AstNodeKind_ADD)) {
                IrExpr tmp = lhs;
                lhs = rhs;
                rhs = tmp;
        }

        ir_expr_materialize(ir_gen, &lhs, NEW_LOC);
        bool use_lit_op = ir_expr_use(ir_gen, &rhs);

        assert(!ir_expr_use(ir_gen, &lhs));

        IrExpr res = ir_expr_new_with_loc(ir_gen, where);
        Op op = node->kind - (AstNodeKind_MUL - Op_MUL);
        op += use_lit_op ? Op_MUL_LIT - Op_MUL : 0;

        ir_emit(
            ir_gen, (Instr){ .op = op, .a = lhs.val, .b = rhs.val, .dst = res.val });

        return res;
    }

    case AstNodeKind_AND:
    case AstNodeKind_OR:
    case AstNodeKind_LT:
    case AstNodeKind_GT:
    case AstNodeKind_EQ: {
        IrLbl f_lbl = ir_mk_lbl(ir_gen);
        IrLbl t_lbl = ir_mk_lbl(ir_gen);
        IrLbl end_lbl = ir_mk_lbl(ir_gen);

        IrTest test =
            ir_gen_test(
                ir_gen, ast, idx,
                (IrCtrl){ .t = t_lbl, .f = f_lbl, .n = f_lbl });

        if (test == IrTest_False) {
            ir_emit(ir_gen, (Instr){ .op = Op_LBL, .a = f_lbl });
            return (IrExpr){ .kind = Ir_Lit, .val = false };
        }
        if (test == IrTest_True) {
            ir_emit(ir_gen, (Instr){ .op = Op_LBL, .a = t_lbl });
            return (IrExpr){ .kind = Ir_Lit, .val = true };
        }

        IrExpr res = ir_expr_new_with_loc(ir_gen, where);

        // TODO: cmov
        ir_emit(ir_gen, (Instr){ .op = Op_LBL, .a = f_lbl });
        ir_emit(ir_gen, (Instr){ .op = Op_MOV_LIT, .a = false, .dst = res.val });
        ir_emit(ir_gen, (Instr){ .op = Op_JMP, .a = end_lbl });
        ir_emit(ir_gen, (Instr){ .op = Op_LBL, .a = t_lbl });
        ir_emit(ir_gen, (Instr){ .op = Op_MOV_LIT, .a = true, .dst = res.val });
        ir_emit(ir_gen, (Instr){ .op = Op_LBL, .a = end_lbl });

        return res;
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
        ir_expr_materialize(ir_gen, &expr, where);

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
            sym_info == NULL ? NEW_LOC : sym_info->ir_info.loc;
        IrExpr expr = ir_gen_expr(ir_gen, ast, asgn.expr, where);
        ir_expr_materialize(ir_gen, &expr, where);

        if (sym_info == NULL) { // global
            ir_expr_use(ir_gen, &expr);
            ir_emit(
                ir_gen,
                (Instr){ .op = Op_SET_GLOBAL, .a = expr.val, .dst = asgn.target });
        }

        return false;
    }

    case AstNodeKind_IF_SMP:
    case AstNodeKind_IF_ALT: {
        AstNodeFull_If if_node = Ast_full_if(ast, idx);

        IrLbl t_lbl = ir_mk_lbl(ir_gen);
        IrLbl f_lbl = ir_mk_lbl(ir_gen);
        IrLbl end_lbl = ir_mk_lbl(ir_gen);

        IrTest test =
            ir_gen_test(
                ir_gen, ast, if_node.cond,
                (IrCtrl){ .t = t_lbl, .f = f_lbl, .n = t_lbl });

        bool then_has_ret = !TEST_HAS(test, True);
        bool else_has_ret = !TEST_HAS(test, False);

        if (TEST_HAS(test, True)) {
            ir_emit(ir_gen, (Instr){ .op = Op_LBL, .a = t_lbl });
            then_has_ret = ir_gen_stmt(ir_gen, ast, if_node.then_b);
        }

        if (TEST_HAS(test, Both) && if_node.else_b != NO_NODE && !then_has_ret) {
            ir_emit(ir_gen, (Instr){ .op = Op_JMP, .a = end_lbl });
        }

        if (TEST_HAS(test, False)) {
            ir_emit(ir_gen, (Instr){ .op = Op_LBL, .a = f_lbl });

            if (if_node.else_b != NO_NODE) {
                else_has_ret = ir_gen_stmt(ir_gen, ast, if_node.else_b);
            }
        }

        if (TEST_HAS(test, Both) && if_node.else_b != NO_NODE && !then_has_ret) {
            ir_emit(ir_gen, (Instr){ .op = Op_LBL, .a = end_lbl });
        }

        return then_has_ret && else_has_ret;
    }

    case AstNodeKind_WHILE: {
        AstNodeFull_While while_node = Ast_full_while(ast, idx);

        IrLbl header_lbl = ir_mk_lbl(ir_gen);
        IrLbl t_lbl = ir_mk_lbl(ir_gen);
        IrLbl f_lbl = ir_mk_lbl(ir_gen);

        ir_emit(ir_gen, (Instr){ .op = Op_LBL, .a = header_lbl });

        IrTest test =
            ir_gen_test(
                ir_gen, ast, while_node.cond,
                (IrCtrl){ .t = t_lbl, .f = f_lbl, .n = t_lbl });

        if (TEST_HAS(test, True)) {
            ir_emit(ir_gen, (Instr){ .op = Op_LBL, .a = t_lbl });

            bool body_has_ret = ir_gen_stmt(ir_gen, ast, while_node.body);
            if (!body_has_ret) {
                ir_emit(ir_gen, (Instr){ .op = Op_JMP, .a = header_lbl });
            }
        }

        if (TEST_HAS(test, False)) {
            ir_emit(ir_gen, (Instr){ .op = Op_LBL, .a = f_lbl });
        }

        return !TEST_HAS(test, False);
    }

    case AstNodeKind_RET: {
        NodeIdx expr_idx = node->data.lhs;

        IrExpr ret_val = (IrExpr){ .kind = Ir_Lit, .val = 0 };
        if (expr_idx != NO_NODE) {
            ret_val = ir_gen_expr(ir_gen, ast, expr_idx, 0);
        }

        if (ir_expr_use(ir_gen, &ret_val)) {
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
