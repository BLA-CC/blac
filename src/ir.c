#include "ir.h"

#include <stdint.h>

#include "ast.h"
#include "str_pool.h"
#include "sym_table.h"
#include "vec.h"
#include "consteval.h"

Vec_Impl(IrVar);
Vec_Impl(Instr);
Vec_Impl(Func);
Vec_Impl(Global);

void ir_new_func(IrGen *ir_gen, StrIdx name, uint32_t arity) {
    Func new_func = { .instrs = { 0 }, .name = name, .locals = 0, .arity = arity };
    FuncVec_push(&ir_gen->ir.funcs, new_func);
    ir_gen->cur_func = &ir_gen->ir.funcs.elems[ir_gen->ir.funcs.len - 1];
}

IrVar ir_mk_var(IrGen *ir_gen) {
    ir_gen->vstack_top += 1;
    if (ir_gen->cur_func->locals < ir_gen->vstack_top) {
        ir_gen->cur_func->locals = ir_gen->vstack_top;
    }
    return ir_gen->vstack_top;
}

void ir_free_var(IrGen *ir_gen, IrVar v) {
    assert(v == ir_gen->vstack_top);
    ir_gen->vstack_top--;
}

uint32_t ir_mk_label(IrGen *ir_gen) {
    return ir_gen->label_gen++;
}

static inline void ir_emit(IrGen *ir_gen, Instr instr) {
    InstrVec_push(&ir_gen->cur_func->instrs, instr);
}

////////////////////////////////////////////////////////////////////////////////

static void ir_gen_expr(IrGen *ir_gen, Ast ast, NodeIdx idx, IrVar where);

static void ir_gen_meth_call(IrGen *ir_gen, Ast ast, NodeIdx idx, IrVar where) {
    AstNodeFull_MethCall meth_call = Ast_full_meth_call(ast, idx);
    uint32_t argc = meth_call.args_end - meth_call.args_begin;

    // compile arguments into vector
    // HACK: this is obviously unnecesary: arguments don't need to be saved
    //       since they are in the last `argc` slots of the vstack.
    //       However, this enables some sanity checks and future optimizations.
    IrVarVec args = { 0 };
    for (uint32_t i = meth_call.args_begin; i < meth_call.args_end; i++) {
        IrVar tmp = ir_mk_var(ir_gen);
        ir_gen_expr(ir_gen, ast, i, tmp);
        IrVarVec_push(&args, tmp);
    }

    // emit and free arguments
    for (int32_t i = argc - 1; i >= 0; i--) {
        ir_emit(ir_gen, (Instr){ .op = Op_ARG, .a = args.elems[i], .dst = i });
        ir_free_var(ir_gen, args.elems[i]);
    }

    IrVarVec_free(&args);

    // emit call into requested location
    ir_emit(
        ir_gen,
        (Instr){ .op = Op_CALL,
                 .a = meth_call.meth_ident,
                 .b = argc,
                 .dst = where });
}

static void ir_gen_expr(IrGen *ir_gen, Ast ast, NodeIdx idx, IrVar where) {
    AstNode *node = &ast.nodes[idx];

    switch (node->kind) {
    case AstNodeKind_METH_CALL:
        ir_gen_meth_call(ir_gen, ast, idx, where);
        break;

    case AstNodeKind_VAR: {
        StrIdx ident = node->data.lhs;
        SymInfo *sym_info = symtable_get_symbol(&ir_gen->sym_table, ident);

        Instr get_instr;
        if (sym_info != NULL) {
            get_instr = (Instr){ .op = Op_MOV_VAR,
                                 .a = sym_info->ir_info.loc,
                                 .dst = where };
        } else { // global
            get_instr =
                (Instr){ .op = Op_GET_GLOBAL, .a = ident, .dst = where };
        }
        ir_emit(ir_gen, get_instr);

    } break;

    case AstNodeKind_INT_LIT:
    case AstNodeKind_BOOL_LIT: {
        int32_t val = node->data.lhs;
        ir_emit(ir_gen, (Instr){ .op = Op_MOV_LIT, .a = val, .dst = where });
    } break;

    case AstNodeKind_UNM:
    case AstNodeKind_NEG: {
        NodeIdx operand = node->data.lhs;

        ir_gen_expr(ir_gen, ast, operand, where);

        ir_emit(
            ir_gen,
            (Instr){ // HACK: this is awesome.
                     .op = node->kind - (AstNodeKind_UNM - Op_UNM),
                     .a = where,
                     .dst = where });
    } break;

    case AstNodeKind_MUL:
    case AstNodeKind_DIV:
    case AstNodeKind_MOD:
    case AstNodeKind_ADD:
    case AstNodeKind_SUB:
    case AstNodeKind_LT:
    case AstNodeKind_GT:
    case AstNodeKind_EQ: {
        NodeIdx lhs = node->data.lhs;
        NodeIdx rhs = node->data.rhs;

        IrVar tmp = ir_mk_var(ir_gen);
        ir_gen_expr(ir_gen, ast, lhs, tmp);
        ir_gen_expr(ir_gen, ast, rhs, where);

        ir_emit(
            ir_gen,
            (Instr){ // HACK: this is awesome.
                     .op = node->kind - (AstNodeKind_MUL - Op_MUL),
                     .a = tmp,
                     .b = where,
                     .dst = where });

        ir_free_var(ir_gen, tmp);
    } break;

    case AstNodeKind_AND:
    case AstNodeKind_OR: {
        NodeIdx lhs = node->data.lhs;
        NodeIdx rhs = node->data.rhs;

        uint32_t label = ir_mk_label(ir_gen);
        IrVar tmp = ir_mk_var(ir_gen);

        ir_gen_expr(ir_gen, ast, lhs, tmp);

        ir_emit(
            ir_gen,
            (Instr){
                .op = node->kind == AstNodeKind_AND ? Op_JMP_IF_F : Op_JMP_IF_T,
                .a = tmp,
                .b = label,
            });

        ir_gen_expr(ir_gen, ast, rhs, tmp);

        ir_emit(ir_gen, (Instr){ .op = Op_LABEL, .a = label });
        ir_emit(ir_gen, (Instr){ .op = Op_MOV_VAR, .dst = where, .a = tmp });
        ir_free_var(ir_gen, tmp);
    } break;

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

        IrVar new_loc = ir_mk_var(ir_gen);
        ir_gen_expr(ir_gen, ast, var_decl.init_expr, new_loc);

        symtable_put_symbol(
            &ir_gen->sym_table,
            var_decl.ident,
            (TypeInfo){ 0 },
            (IrInfo){ .loc = new_loc });

        return false;
    }

    case AstNodeKind_ASGN: {
        AstNodeFull_Asgn asgn = Ast_full_asgn(ast, idx);
        SymInfo *sym_info =
            symtable_get_symbol(&ir_gen->sym_table, asgn.target);

        IrVar where =
            sym_info == NULL ? ir_mk_var(ir_gen) : sym_info->ir_info.loc;
        ir_gen_expr(ir_gen, ast, asgn.expr, where);

        if (sym_info == NULL) { // global
            ir_emit(
                ir_gen,
                (Instr){ .op = Op_SET_GLOBAL, .a = where, .dst = asgn.target });
            ir_free_var(ir_gen, where);
        }

        return false;
    }

    case AstNodeKind_IF_SMP:
    case AstNodeKind_IF_ALT: {
        AstNodeFull_If if_node = Ast_full_if(ast, idx);

        uint32_t l_end = ir_mk_label(ir_gen);
        uint32_t l_else = ir_mk_label(ir_gen);

        IrVar tmp = ir_mk_var(ir_gen);
        ir_gen_expr(ir_gen, ast, if_node.cond, tmp);

        ir_emit(ir_gen, (Instr){ .op = Op_JMP_IF_F, .a = tmp, .b = l_else });
        ir_free_var(ir_gen, tmp);

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

        IrVar tmp = ir_mk_var(ir_gen);
        ir_gen_expr(ir_gen, ast, while_node.cond, tmp);

        uint32_t l_escape = ir_mk_label(ir_gen);

        ir_emit(ir_gen, (Instr){ .op = Op_JMP_IF_F, .a = tmp, .b = l_escape });
        ir_free_var(ir_gen, tmp);

        bool block_has_ret = ir_gen_stmt(ir_gen, ast, while_node.body);

        if (!block_has_ret) {
            ir_emit(ir_gen, (Instr){ .op = Op_JMP, .a = l_test });
        }

        ir_emit(ir_gen, (Instr){ .op = Op_LABEL, .a = l_escape });

        return false;
    }

    case AstNodeKind_RET: {
        NodeIdx expr_idx = node->data.lhs;

        Instr ret_instr = (Instr){ .op = Op_RET, .a = 0 };
        if (expr_idx != NO_NODE) {
            IrVar tmp = ir_mk_var(ir_gen);
            ir_gen_expr(ir_gen, ast, expr_idx, tmp);
            ret_instr.a = tmp;
            ir_free_var(ir_gen, tmp);
        }
        ir_emit(ir_gen, ret_instr);

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

        IrVar param_var = ir_mk_var(ir_gen);
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
