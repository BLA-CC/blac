#include "ir.h"
#include "ast.h"
#include "str_pool.h"
#include "sym_table.h"
#include "vec.h"
#include <stdint.h>

Vec_Impl(Instr);
Vec_Impl(Func);

void ir_new_func(IrGen *ir_gen) {
    Func new_func = { 0 };
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

static void ir_gen_expr(IrGen *ir_gen, Ast ast, NodeIdx idx) {
    AstNode *node = &ast.nodes[idx];

    switch (node->kind) {
    case AstNodeKind_METH_CALL: {
    } break;
    case AstNodeKind_VAR: {
        StrIdx ident = node->data.lhs;
        SymInfo *sym_info = symtable_get_symbol(&ir_gen->sym_table, ident);

        Instr mov_instr = { .op = Op_MOV_VAR,
                            .a = sym_info->ir_info.loc,
                            .dst = ir_mk_var(ir_gen) };
        InstrVec_push(&ir_gen->cur_func->instrs, mov_instr);
    } break;
    case AstNodeKind_INT_LIT:
    case AstNodeKind_BOOL_LIT: {
        int32_t val = node->data.lhs;

        Instr mov_instr = { .op = Op_MOV_LIT,
                            .a = val,
                            .dst = ir_mk_var(ir_gen) };
        InstrVec_push(&ir_gen->cur_func->instrs, mov_instr);
    } break;
    case AstNodeKind_UNM:
    case AstNodeKind_NEG: {
        NodeIdx operand = node->data.lhs;

        ir_gen_expr(ir_gen, ast, operand);

        Instr unop_instr = (Instr){
            // HACK: this is awesome.
            .op = node->kind - (AstNodeKind_UNM - Op_UNM),
            .a = ir_gen->vstack_top,
            .dst = ir_gen->vstack_top,
        };
        InstrVec_push(&ir_gen->cur_func->instrs, unop_instr);
    } break;
    case AstNodeKind_MUL:
    case AstNodeKind_DIV:
    case AstNodeKind_MOD:
    case AstNodeKind_ADD:
    case AstNodeKind_SUB:
    case AstNodeKind_LT:
    case AstNodeKind_GT:
    case AstNodeKind_EQ:
    case AstNodeKind_AND:
    case AstNodeKind_OR: {
        NodeIdx lhs = node->data.lhs;
        NodeIdx rhs = node->data.rhs;

        ir_gen_expr(ir_gen, ast, lhs);
        ir_gen_expr(ir_gen, ast, rhs);

        Instr binop_instr = (Instr){
            // HACK: this is awesome.
            .op = node->kind - (AstNodeKind_MUL - Op_MUL),
            .a = ir_gen->vstack_top - 1,
            .b = ir_gen->vstack_top,
            .dst = ir_gen->vstack_top - 1,
        };
        InstrVec_push(&ir_gen->cur_func->instrs, binop_instr);

        ir_free_var(ir_gen, binop_instr.b);
    } break;
    default:
        unreachable;
    }
}

static void ir_gen_stmt(IrGen *ir_gen, Ast ast, NodeIdx idx) {
    AstNode *node = &ast.nodes[idx];

    switch (node->kind) {
    case AstNodeKind_BLOCK: {
        AstNodeFull_List block = Ast_full_block(ast, idx);

        symtable_push_scope(&ir_gen->sym_table);
        for (NodeIdx i = block.begin; i < block.end; i++) {
            ir_gen_stmt(ir_gen, ast, i);
        }
        symtable_pop_scope(&ir_gen->sym_table, &ir_gen->vstack_top);
    } break;
    /* case AstNodeKind_VAR_DECL: */
    // TODO: (tyck) assert todo se inicializa o valor x defecto?
    case AstNodeKind_VAR_DECL_INIT: {
        AstNodeFull_VarDecl var_decl = Ast_full_var_decl(ast, idx);

        ir_gen_expr(ir_gen, ast, var_decl.init_expr);

        symtable_put_symbol(
            &ir_gen->sym_table,
            var_decl.ident,
            (TypeInfo){ 0 },
            (IrInfo){ .loc = ir_gen->vstack_top });
    } break;
    case AstNodeKind_ASGN: {
        AstNodeFull_Asgn asgn = Ast_full_asgn(ast, idx);
        SymInfo *sym_info =
            symtable_get_symbol(&ir_gen->sym_table, asgn.target);
        IrVar loc = sym_info->ir_info.loc;

        ir_gen_expr(ir_gen, ast, asgn.expr);

        Instr mov_instr =
            (Instr){ .op = Op_MOV_VAR, .a = ir_gen->vstack_top, .dst = loc };
        InstrVec_push(&ir_gen->cur_func->instrs, mov_instr);

        ir_free_var(ir_gen, mov_instr.a);
    } break;
    case AstNodeKind_IF_SMP:
    case AstNodeKind_IF_ALT: {
    } break;
    case AstNodeKind_WHILE: {
    } break;
    case AstNodeKind_RET: {
        NodeIdx expr_idx = node->data.lhs;

        Instr ret_instr = (Instr){ .op = Op_RET, .a = 0 };
        if (expr_idx != NO_NODE) {
            ir_gen_expr(ir_gen, ast, expr_idx);
            ret_instr.a = ir_gen->vstack_top;
            ir_free_var(ir_gen, ret_instr.a);
        }
        InstrVec_push(&ir_gen->cur_func->instrs, ret_instr);
    } break;
    case AstNodeKind_METH_CALL: {
    } break;
    default:
        unreachable;
    }
}

static void ir_gen_meth_body(IrGen *ir_gen, Ast ast, NodeIdx idx) {
    ir_gen_stmt(ir_gen, ast, idx);

    Instr ret = (Instr){ .op = Op_RET, .a = 0 };
    InstrVec_push(&ir_gen->cur_func->instrs, ret);
}

static void ir_gen_meth_decl(IrGen *ir_gen, Ast ast, NodeIdx idx) {
    AstNodeFull_MethDecl meth_decl = Ast_full_meth_decl(ast, idx);

    symtable_put_symbol(
        &ir_gen->sym_table, meth_decl.ident, (TypeInfo){ 0 }, (IrInfo){ 0 });

    if (meth_decl.body == NO_NODE) {
        return;
    }

    ir_new_func(ir_gen);
    ir_gen->cur_func->name = meth_decl.ident;

    AstNodeFull_List params = Ast_full_list(ast, meth_decl.params);

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

static void ir_gen_prog(IrGen *ir_gen, Ast ast) {
    AstNodeFull_List prog = Ast_full_prog(ast);

    for (uint32_t i = prog.begin; i < prog.end; i++) {
        AstNode *node = &ast.nodes[i];
        switch (node->kind) {
        case AstNodeKind_VAR_DECL_INIT:
        case AstNodeKind_VAR_DECL:
            /* TODO: ir_gen_global(ir_gen, ast, strs, i); */
            panic("unimplemented (compile time evaluation)");
            break;

        case AstNodeKind_METH_DECL_IMPL:
        case AstNodeKind_METH_DECL:
            ir_gen_meth_decl(ir_gen, ast, i);
            break;

        default:
            unreachable;
        }
    }
}

Ir mk_ir(const Ast ast, StrPool strs) {
    (void)strs;
    IrGen ir_gen = { 0 };

    ir_gen_prog(&ir_gen, ast);

    SymInfoVec_free(&ir_gen.sym_table.symbols);

    return ir_gen.ir;
}
