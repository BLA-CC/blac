#include "ast.h"
#include "str_pool.h"
#include "ast_visitor.h"
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>

static const char *_str_un_op(UnOp op) {
    switch (op) {
    case UnOp_NEG:
        return "!";
    case UnOp_UNM:
        return "-";
    }
    return "INVALID";
}

static const char *_str_bin_op(BinOp op) {
    switch (op) {
    case BinOp_MUL:
        return "*";

    case BinOp_DIV:
        return "/";

    case BinOp_MOD:
        return "%";

    case BinOp_ADD:
        return "+";

    case BinOp_SUB:
        return "-";

    case BinOp_LT:
        return "<";

    case BinOp_GT:
        return ">";

    case BinOp_EQ:
        return "==";

    case BinOp_AND:
        return "&&";

    case BinOp_OR:
        return "||";
    }
    return "INVALID";
}

static const char *_str_type(Type type) {
    switch (type) {
    case Type_VOID:
        return "void";
    case Type_INT:
        return "integer";
    case Type_BOOL:
        return "bool";
    default:
        return "INVALID Type";
    }
}

static const char *_str_bool(bool val) {
    if (val) {
        return "true";
    } else {
        return "false";
    }
}

void display_list(Visitor v, NodeIdx begin, NodeIdx end) {
    for (NodeIdx i = begin; i < end; i++) {
        ast_visit(v, i);
    }
}

void display_prog(Visitor v, AstNodeFull_List prog_n) {
    FILE *stream = (FILE *)visitor_get_ctx(v);
    fprintf(stream, "program {\n");
    display_list(v, prog_n.begin, prog_n.end);
    fprintf(stream, "}\n");
}

void display_block(Visitor v, AstNodeFull_List block_n) {
    display_list(v, block_n.begin, block_n.end);
}

void display_var_decl(Visitor v, AstNodeFull_VarDecl var_decl_n) {
    FILE *stream = (FILE *)visitor_get_ctx(v);
    StrPool strs = visitor_get_strs(v);

    fprintf(
        stream,
        "%s %s = ",
        _str_type(var_decl_n.type),
        StrPool_get(&strs, var_decl_n.ident) );
    ast_visit(v, var_decl_n.init_expr);
    fprintf(stream, ";\n");
}

void display_meth_decl(Visitor v, AstNodeFull_MethDecl meth_decl_n) {
    FILE *stream = (FILE *)visitor_get_ctx(v);
    StrPool strs = visitor_get_strs(v);

    fprintf(
        stream,
        "%s %s(",
        _str_type(meth_decl_n.ret_type),
        StrPool_get(&strs, meth_decl_n.ident) );
    
    display_list(v, meth_decl_n.params_begin, meth_decl_n.params_end);
    fprintf(stream, ")");

    if (meth_decl_n.body != NO_NODE) {
        fprintf(stream, "{\n");
        ast_visit(v, meth_decl_n.body);
        fprintf(stream, "}");
    } else {
        fprintf(stream, " extern;");
    }

    fprintf(stream, "\n");

}

void display_param(Visitor v, Type type, StrIdx ident) {
    FILE *stream = (FILE *)visitor_get_ctx(v);
    StrPool strs = visitor_get_strs(v);

    fprintf(stream, "%s %s", _str_type(type), StrPool_get(&strs, ident));
}


void display_if(Visitor v, AstNodeFull_If if_n) {
    FILE *stream = (FILE *)visitor_get_ctx(v);
    fprintf(stream, "if (");
    ast_visit(v, if_n.cond);
    fprintf(stream, ") then {\n");
    ast_visit(v, if_n.then_b);
    fprintf(stream, "}");

    if (if_n.else_b != NO_NODE) {
        fprintf(stream, " else {\n");
        ast_visit(v, if_n.else_b);
        fprintf(stream, "}");
    }

    fprintf(stream, "\n");
}

void display_while(Visitor v, AstNodeFull_While while_n) {
    FILE *stream = (FILE *)visitor_get_ctx(v);
    fprintf(stream, "while (");
    ast_visit(v, while_n.cond);
    fprintf(stream, ") {\n");
    ast_visit(v, while_n.body);
    fprintf(stream, "}\n");
}

void display_ret(Visitor v, NodeIdx expr_idx) {
    FILE *stream = (FILE *)visitor_get_ctx(v);

    fprintf(stream, "return");
    if (expr_idx != NO_NODE) {
        fprintf(stream, " ");
        ast_visit(v, expr_idx);
    }
    fprintf(stream, ";\n");
}


void display_meth_call(Visitor v, AstNodeFull_MethCall meth_call_n) {
    FILE *stream = (FILE *)visitor_get_ctx(v);
    StrPool strs = visitor_get_strs(v);
    fprintf(stream, "%s(",StrPool_get(&strs, meth_call_n.meth_ident));
    display_list(v, meth_call_n.args_begin, meth_call_n.args_end);
    fprintf(stream, ")");
}

void display_var(Visitor v, StrIdx ident) {
    StrPool strs = visitor_get_strs(v);

    fprintf((FILE *)visitor_get_ctx(v), "%s", StrPool_get(&strs, ident));
}

void display_int_lit(Visitor v, uint32_t val) {
    fprintf((FILE *)visitor_get_ctx(v), "%d", val);
}

void display_bool_lit(Visitor v, bool val) {
    fprintf((FILE *)visitor_get_ctx(v), "%s", _str_bool(val));
}

void display_asgn(Visitor v, AstNodeFull_Asgn asgn_n) {
    FILE *stream = (FILE *)visitor_get_ctx(v);
    StrPool strs = visitor_get_strs(v);

    fprintf(stream, "%s = ", StrPool_get(&strs, asgn_n.target));
    ast_visit(v, asgn_n.expr);
    fprintf(stream, ";\n");
}


void display_unop(Visitor v, AstNodeFull_UnOp unop_n) {
    FILE *stream = (FILE *)visitor_get_ctx(v);
    fprintf(stream, "%s ", _str_un_op(unop_n.op));
    ast_visit(v, unop_n.arg);
}

void display_binop(Visitor v, AstNodeFull_BinOp binop_n) {
    FILE *stream = (FILE *)visitor_get_ctx(v);

    fprintf(stream, "(");
    ast_visit(v, binop_n.lhs);
    fprintf(stream, " %s ", _str_bin_op(binop_n.op));
    ast_visit(v, binop_n.rhs);
    fprintf(stream, ")");
}



void ast_display(const Ast ast, NodeIdx idx, StrPool strs, FILE *stream){
    Visitor visitor = init_visitor(
        ast,
        strs,
        (void *)stream,
        display_prog,
        display_block,
        display_var_decl,
        display_meth_decl,
        display_param,
        display_asgn,
        display_if,
        display_while,
        display_ret,
        display_meth_call,
        display_var,
        display_int_lit,
        display_bool_lit,
        display_unop,
        display_binop
    );

    ast_visit(visitor, idx);
    fprintf(stream, "\n");

    visitor_release(visitor);
}
