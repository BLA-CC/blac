#include "ir.h"
#include "ast.h"
#include "str_pool.h"
#include "ast_visitor.h"
#include "sym_table.h"
#include "vec.h"
#include <stdint.h>

Vec_Impl(Instr);
Vec_Impl(Func);

void ir_new_func(IrGen *ir_gen) {
    Func new_func = {0};
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


void ir_prog(AstVisitor *v, AstNodeFull_List prog_n) {
    for (NodeIdx i = prog_n.begin; i < prog_n.end; i++) {
        ast_visit(v, i);
    }
}

void ir_block(AstVisitor *v, AstNodeFull_List block_n) {
    IrGen *ir_gen = (IrGen *)v->ctx;

    symtable_push_scope(&ir_gen->sym_table);

    for (NodeIdx i = block_n.begin; i < block_n.end; i++) {
        ast_visit(v, i);
    }

    symtable_pop_scope(&ir_gen->sym_table, &ir_gen->vstack_top);
}

void ir_var_decl(AstVisitor *v, AstNodeFull_VarDecl var_decl_n) {
    IrGen *ir_gen = (IrGen *)v->ctx;

    IrVar new_var = ir_mk_var(ir_gen);
    symtable_put_symbol(
        &ir_gen->sym_table, var_decl_n.ident,
        (TypeInfo){ 0 },
        (IrInfo){.loc = new_var}
    );

    ast_visit(v, var_decl_n.init_expr);
    Instr var_decl_instr = (Instr) {
        .op = Op_MOV_VAR,
        .a = ir_gen->vstack_top,
        .dst = new_var
    };
    InstrVec_push(&ir_gen->cur_func->instrs, var_decl_instr);
    ir_free_var(ir_gen, var_decl_instr.a);
}

void ir_meth_decl(AstVisitor *v, AstNodeFull_MethDecl meth_decl_n) {
    IrGen *ir_gen = (IrGen *)v->ctx;
    ir_new_func(ir_gen);
    uint32_t label = ir_mk_label(ir_gen);
    symtable_put_symbol(
            &ir_gen->sym_table,
            meth_decl_n.ident,
            (TypeInfo){ 0 },
            (IrInfo) { 0 });

    ir_gen->cur_func->name = meth_decl_n.ident;

    AstNodeFull_List params = Ast_full_list(v->ast, meth_decl_n.params);
    for (NodeIdx i = params.begin; i < params.end; i++) {
        ast_visit(v, i);
    }

    ast_visit(v, meth_decl_n.body);
}

void ir_param(AstVisitor *v, Type type, StrIdx ident) {
    IrGen *ir_gen = (IrGen *)v->ctx;

    IrVar new_param = ir_mk_var(ir_gen);
    symtable_put_symbol(
            &ir_gen->sym_table,
            ident,
            (TypeInfo){ .base = type },
            (IrInfo) { .loc = new_param });

}

void ir_asgn(AstVisitor *v, AstNodeFull_Asgn asgn_n) {
    IrGen *ir_gen = (IrGen *)v->ctx;

    SymInfo *sym_info = symtable_get_symbol(&ir_gen->sym_table, asgn_n.target);
    uint32_t loc = sym_info->ir_info.loc;
    ast_visit(v, asgn_n.expr);

    Instr mov_instr = (Instr) {
        .op = Op_MOV_LIT,
        .a = ir_gen->vstack_top,
        .dst = loc
    };

    ir_free_var(ir_gen, mov_instr.a);
}

void ir_if(AstVisitor *v, AstNodeFull_If if_n) {
    // TODO
}

void ir_while(AstVisitor *v, AstNodeFull_While while_n) {
    // TODO
}

void ir_ret(AstVisitor *v, NodeIdx expr_idx) {
    IrGen *ir_gen = (IrGen *)v->ctx;
    Instr ret_instr = (Instr){ .op = Op_RET };

    if (expr_idx != NO_NODE) {
        ast_visit(v, expr_idx);
        ret_instr.a = ir_gen->vstack_top;
        ir_free_var(ir_gen, ret_instr.a);
    }

    InstrVec_push(&ir_gen->cur_func->instrs, ret_instr);
}

void ir_meth_call(AstVisitor *v, AstNodeFull_MethCall meth_call_n) {
    // TODO
}

void ir_var(AstVisitor *v, StrIdx ident) {
    IrGen *ir_gen = (IrGen *)v->ctx;
    SymInfo *sym_info = symtable_get_symbol(&ir_gen->sym_table, ident);
    
    Instr mov_instr = {
        .op = Op_MOV_VAR,
        .a = sym_info->ir_info.loc,
        .dst = ir_mk_var(ir_gen)
    };
    InstrVec_push(&ir_gen->cur_func->instrs, mov_instr);
}

void ir_int_lit(AstVisitor *v, uint32_t val) {
    IrGen *ir_gen = (IrGen *)v->ctx;

   Instr mov_instr = {
        .op = Op_MOV_LIT,
        .a = val,
        .dst = ir_mk_var(ir_gen)
    };
    InstrVec_push(&ir_gen->cur_func->instrs, mov_instr);
}

void ir_bool_lit(AstVisitor *v, bool val) {
    IrGen *ir_gen = (IrGen *)v->ctx;

    Instr mov_instr = {
        .op = Op_MOV_LIT,
        .a = val,
        .dst = ir_mk_var(ir_gen)
    };
    InstrVec_push(&ir_gen->cur_func->instrs, mov_instr);
}

void ir_unop (AstVisitor *v, AstNodeFull_UnOp unop_n) {
    IrGen *ir_gen = (IrGen *)v->ctx;

    ast_visit(v, unop_n.arg);
    Instr unop_instr = { .a = ir_gen->vstack_top, .dst =  ir_mk_var(ir_gen) };

    switch (unop_n.op) {
    case UnOp_UNM:
        unop_instr.op = Op_UNM;
        break;
    case UnOp_NEG:
        unop_instr.op = Op_NEG;
        break;
    }

    InstrVec_push(&ir_gen->cur_func->instrs, unop_instr);
    ir_free_var(ir_gen, unop_instr.a);
}

void ir_binop(AstVisitor *v, AstNodeFull_BinOp binop_n) {
    IrGen *ir_gen = (IrGen *)v->ctx;

    ast_visit(v, binop_n.lhs);

    ast_visit(v, binop_n.rhs);

    Instr binop_instr = (Instr) {
        .a = ir_gen->vstack_top - 1,
        .b = ir_gen->vstack_top,
        .dst = ir_mk_var(ir_gen)
    };

    switch (binop_n.op) {
    case BinOp_MUL:
        binop_instr.op = Op_MUL;;
        break;
    case BinOp_DIV:
        binop_instr.op = Op_DIV;
        break;
    case BinOp_MOD:
        binop_instr.op = Op_MOD;
        break;
    case BinOp_ADD:
        binop_instr.op = Op_ADD;
        break;
    case BinOp_SUB:
        binop_instr.op = Op_SUB;
        break;
    case BinOp_LT:
        binop_instr.op = Op_LT;
        break;
    case BinOp_GT:
        binop_instr.op = Op_GT;
        break;
    case BinOp_EQ:
        binop_instr.op = Op_EQ;
        break;
    case BinOp_AND:
        binop_instr.op = Op_AND;
        break;
    case BinOp_OR:
        binop_instr.op = Op_OR;
        break;
    }
    
    InstrVec_push(&ir_gen->cur_func->instrs, binop_instr);

    ir_free_var(ir_gen, binop_instr.b);
    ir_free_var(ir_gen, binop_instr.a);
}


Ir mk_ir(const Ast ast, StrPool strs) {
    IrGen ir_gen = { 0 };
    AstVisitor visitor = (AstVisitor) {
        ast, strs, &ir_gen, { 0 },
        ir_prog, ir_block, ir_var_decl, ir_meth_decl, ir_param, ir_asgn,
        ir_if, ir_while, ir_ret, ir_meth_call, ir_var, ir_int_lit,
        ir_bool_lit, ir_unop, ir_binop
    };
    ast_visit(&visitor, AST_ROOT);
    return ir_gen.ir;
}
