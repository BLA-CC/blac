#include <stdbool.h>

#include "ast.h"
#include "ast_visitor.h"
#include "common.h"
#include "sym_table.h"

typedef struct {
    SymTable sym_table;
    Type type;
    Type ret_type;
    bool had_error;
} Tyck;

static void tyck_prog(AstVisitor *v, AstNodeFull_List prog_n) {
    for (NodeIdx i = prog_n.begin; i < prog_n.end; i++) {
        ast_visit(v, i);
    }
}

static void tyck_block(AstVisitor *v, AstNodeFull_List block_n) {
    Tyck *tyck = (Tyck *)v->ctx;

    symtable_push_scope(&tyck->sym_table);

    for (NodeIdx i = block_n.begin; i < block_n.end; i++) {
        ast_visit(v, i);
    }

    symtable_pop_scope(&tyck->sym_table);
}

static void tyck_var_decl(AstVisitor *v, AstNodeFull_VarDecl var_decl_n) {
    Tyck *tyck = (Tyck *)v->ctx;

    ast_visit(v, var_decl_n.init_expr);

    if (!symtable_put_symbol(
            &tyck->sym_table,
            var_decl_n.init_expr,
            (TypeInfo){ .base = var_decl_n.type, .params = NO_NODE })) {
        tyck->had_error = true;
        // FIXME: report (double decl)

        // if it is a double declaration, a type mismatch cannot be reported
        return;
    }

    if (tyck->type != var_decl_n.type) {
        tyck->had_error = true;
        // FIXME: report (mismatch)
    }
}

static void tyck_meth_decl(AstVisitor *v, AstNodeFull_MethDecl meth_decl_n) {
    Tyck *tyck = (Tyck *)v->ctx;

    // before checking, to allow recursive calls
    if (!symtable_put_symbol(
            &tyck->sym_table,
            meth_decl_n.ident,
            (TypeInfo){ .base = meth_decl_n.ret_type,
                        .params = meth_decl_n.params })) {
        tyck->had_error = true;
        // FIXME: report (double decl (method))
    }

    // Nothing to check if there is no body
    if (meth_decl_n.body == NO_NODE) {
        return;
    }

    symtable_push_scope(&tyck->sym_table); // parameter scope

    AstNodeFull_List params = Ast_full_list(v->ast, meth_decl_n.params);
    for (NodeIdx i = params.begin; i < params.end; i++) {
        ast_visit(v, i);
    }

    tyck->ret_type = meth_decl_n.ret_type;

    ast_visit(v, meth_decl_n.body);

    symtable_pop_scope(&tyck->sym_table); // parameter scope
}

static void tyck_param(AstVisitor *v, Type type, StrIdx ident) {
    Tyck *tyck = (Tyck *)v->ctx;

    if (!symtable_put_symbol(
            &tyck->sym_table,
            ident,
            (TypeInfo){ .base = type, .params = NO_NODE })) {
        tyck->had_error = true;
        // FIXME: report (double decl param)
    }
}

static void tyck_if(AstVisitor *v, AstNodeFull_If if_n) {
    Tyck *tyck = (Tyck *)v->ctx;

    ast_visit(v, if_n.cond);
    if (tyck->type != Type_BOOL) {
        tyck->had_error = true;
        // FIXME: report (mismatch)
    }

    ast_visit(v, if_n.then_b);

    if (if_n.else_b != NO_NODE) {
        ast_visit(v, if_n.else_b);
    }
}

static void tyck_while(AstVisitor *v, AstNodeFull_While while_n) {
    Tyck *tyck = (Tyck *)v->ctx;

    ast_visit(v, while_n.cond);
    if (tyck->type != Type_BOOL) {
        // FIXME: report (mismatch)
        tyck->had_error = true;
    }

    ast_visit(v, while_n.body);
}

static void tyck_ret(AstVisitor *v, NodeIdx expr_n) {
    Tyck *tyck = (Tyck *)v->ctx;

    if (expr_n != NO_NODE) {
        ast_visit(v, expr_n);

        if (tyck->type != tyck->ret_type) {
            tyck->had_error = true;
            // FIXME: report (mismatch (return value))
        }
    } else if (tyck->ret_type != Type_VOID) {
        tyck->had_error = true;
        // FIXME: report (mismatch (return value))
    }
}

static void tyck_meth_call(AstVisitor *v, AstNodeFull_MethCall meth_call_n) {
    Tyck *tyck = (Tyck *)v->ctx;

    SymInfo *sym_info =
        symnode_get_symbol(&tyck->sym_table, meth_call_n.meth_ident);

    if (sym_info == NULL) {
        tyck->had_error = true;
        // FIXME: report (undefined method)
        tyck->type = Type_NONE;
        return;
    }

    if (sym_info->type_info.params == NO_NODE) {
        tyck->had_error = true;
        // FIXME: report (cannot call a basic type)
        tyck->type = Type_NONE;
        return;
    }

    AstNodeFull_List args = { meth_call_n.args_begin, meth_call_n.args_end };
    AstNodeFull_List params = Ast_full_list(v->ast, sym_info->type_info.params);
    uint32_t args_len = args.end - args.begin;
    uint32_t params_len = params.end - params.begin;

    if (args_len != params_len) {
        tyck->had_error = true;
        // FIXME: report (argument count error)
        tyck->type = Type_NONE;
        return;
    }

    Type ret_type = sym_info->type_info.base;

    for (uint32_t i = 0; i < params_len; i++) {
        ast_visit(v, args.begin + i);

        if (tyck->type == Type_NONE) {
            return;
        }

        if (tyck->type != v->ast.nodes[params.begin + i].data.lhs) {
            tyck->had_error = true;
            ret_type = Type_NONE;
            // FIXME: report (mismatch (argument))
        }
    }

    tyck->type = ret_type;
}

static void tyck_var(AstVisitor *v, StrIdx ident) {
    Tyck *tyck = (Tyck *)v->ctx;

    SymInfo *sym_info = symnode_get_symbol(&tyck->sym_table, ident);

    if (sym_info == NULL) {
        tyck->had_error = true;
        // FIXME: report (undefined variable)
        tyck->type = Type_NONE;
        return;
    }

    if (sym_info->type_info.params != NO_NODE) {
        tyck->had_error = true;
        // FIXME: report (methods cannot be used as values)
        tyck->type = Type_NONE;
        return;
    }

    tyck->type = sym_info->type_info.base;
}

static void tyck_int_lit(AstVisitor *v, uint32_t val) {
    (void)val;
    Tyck *tyck = (Tyck *)v->ctx;

    tyck->type = Type_INT;
}

static void tyck_bool_lit(AstVisitor *v, bool val) {
    (void)val;
    Tyck *tyck = (Tyck *)v->ctx;

    tyck->type = Type_BOOL;
}

static void tyck_asgn(AstVisitor *v, AstNodeFull_Asgn asgn_n) {
    Tyck *tyck = (Tyck *)v->ctx;

    SymInfo *sym_info = symnode_get_symbol(&tyck->sym_table, asgn_n.target);

    if (sym_info == NULL) {
        tyck->had_error = true;
        // FIXME: report (undefined)
        return;
    }

    if (sym_info->type_info.params != NO_NODE) {
        tyck->had_error = true;
        // FIXME: report (cannot assign to method)
        return;
    }

    ast_visit(v, asgn_n.expr);

    if (tyck->type != sym_info->type_info.base) {
        tyck->had_error = true;
        // FIXME: report (mismatch)
    }
}

static void tyck_unop(AstVisitor *v, AstNodeFull_UnOp unop_n) {
    Tyck *tyck = (Tyck *)v->ctx;

    Type op_type;
    switch (unop_n.op) {
    case UnOp_UNM:
        op_type = Type_INT;
        break;
    case UnOp_NEG:
        op_type = Type_BOOL;
        break;
    }

    ast_visit(v, unop_n.arg);

    if (tyck->type == Type_NONE) {
        return;
    }

    if (tyck->type != op_type) {
        tyck->had_error = true;
        // FIXME: report (mismatch (operator))
    }
    tyck->type = op_type;
}

static void tyck_binop(AstVisitor *v, AstNodeFull_BinOp binop_n) {
    Tyck *tyck = (Tyck *)v->ctx;

    Type arg_type, res_type;
    switch (binop_n.op) {
    case BinOp_MUL:
    case BinOp_DIV:
    case BinOp_MOD:
    case BinOp_ADD:
    case BinOp_SUB:
        arg_type = Type_INT;
        res_type = Type_INT;
        break;
    case BinOp_LT:
    case BinOp_GT:
    case BinOp_EQ:
        arg_type = Type_INT;
        res_type = Type_BOOL;
        break;
    case BinOp_AND:
    case BinOp_OR:
        arg_type = Type_BOOL;
        res_type = Type_BOOL;
        break;
    }

    ast_visit(v, binop_n.lhs);

    if (tyck->type == Type_NONE) {
        return;
    }

    if (tyck->type != arg_type) {
        tyck->had_error = true;
        // FIXME: report (mismatch (operator))
    }

    ast_visit(v, binop_n.rhs);

    if (tyck->type == Type_NONE) {
        return;
    }

    if (tyck->type != arg_type) {
        tyck->had_error = true;
        // FIXME: report (mismatch (operator))
    }

    tyck->type = res_type;
}

bool tyck(const Ast ast, StrPool strs) {
    Tyck tyck = { 0 };

    AstVisitor visitor = (AstVisitor){
        ast,          strs,          &tyck,          { 0 },          tyck_prog,
        tyck_block,   tyck_var_decl, tyck_meth_decl, tyck_param,     tyck_asgn,
        tyck_if,      tyck_while,    tyck_ret,       tyck_meth_call, tyck_var,
        tyck_int_lit, tyck_bool_lit, tyck_unop,      tyck_binop
    };

    ast_visit(&visitor, AST_ROOT);
    return !tyck.had_error;
}
