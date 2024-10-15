#include "ast_visitor.h"
#include "display.h"

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

void display_list(AstVisitor *v, NodeIdx begin, NodeIdx end) {
    for (NodeIdx i = begin; i < end; i++) {
        ast_visit(v, i);
    }
}

void display_prog(AstVisitor *v, AstNodeFull_List prog_n) {
    FILE *stream = (FILE *)v->ctx;
    fprintf(stream, "program {\n");
    display_list(v, prog_n.begin, prog_n.end);
    fprintf(stream, "}\n");
}

void display_block(AstVisitor *v, AstNodeFull_List block_n) {
    display_list(v, block_n.begin, block_n.end);
}

void display_var_decl(AstVisitor *v, AstNodeFull_VarDecl var_decl_n) {
    FILE *stream = (FILE *)v->ctx;
    StrPool strs = v->strs;

    fprintf(
        stream,
        "%s %s = ",
        _str_type(var_decl_n.type),
        StrPool_get(&strs, var_decl_n.ident) );
    ast_visit(v, var_decl_n.init_expr);
    fprintf(stream, ";\n");
}

void display_meth_decl(AstVisitor *v, AstNodeFull_MethDecl meth_decl_n) {
    FILE *stream = (FILE *)v->ctx;
    StrPool strs = v->strs;

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

void display_param(AstVisitor *v, Type type, StrIdx ident) {
    FILE *stream = (FILE *)v->ctx;
    StrPool strs = v->strs;

    fprintf(stream, "%s %s", _str_type(type), StrPool_get(&strs, ident));
}


void display_if(AstVisitor *v, AstNodeFull_If if_n) {
    FILE *stream = (FILE *)v->ctx;
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

void display_while(AstVisitor *v, AstNodeFull_While while_n) {
    FILE *stream = (FILE *)v->ctx;
    fprintf(stream, "while (");
    ast_visit(v, while_n.cond);
    fprintf(stream, ") {\n");
    ast_visit(v, while_n.body);
    fprintf(stream, "}\n");
}

void display_ret(AstVisitor *v, NodeIdx expr_idx) {
    FILE *stream = (FILE *)v->ctx;

    fprintf(stream, "return");
    if (expr_idx != NO_NODE) {
        fprintf(stream, " ");
        ast_visit(v, expr_idx);
    }
    fprintf(stream, ";\n");
}


void display_meth_call(AstVisitor *v, AstNodeFull_MethCall meth_call_n) {
    FILE *stream = (FILE *)v->ctx;
    StrPool strs = v->strs;
    fprintf(stream, "%s(",StrPool_get(&strs, meth_call_n.meth_ident));
    display_list(v, meth_call_n.args_begin, meth_call_n.args_end);
    fprintf(stream, ")");
}

void display_var(AstVisitor *v, StrIdx ident) {
    StrPool strs = v->strs;

    fprintf((FILE *)v->ctx, "%s", StrPool_get(&strs, ident));
}

void display_int_lit(AstVisitor *v, uint32_t val) {
    fprintf((FILE *)v->ctx, "%d", val);
}

void display_bool_lit(AstVisitor *v, bool val) {
    fprintf((FILE *)v->ctx, "%s", _str_bool(val));
}

void display_asgn(AstVisitor *v, AstNodeFull_Asgn asgn_n) {
    FILE *stream = (FILE *)v->ctx;
    StrPool strs = v->strs;

    fprintf(stream, "%s = ", StrPool_get(&strs, asgn_n.target));
    ast_visit(v, asgn_n.expr);
    fprintf(stream, ";\n");
}


void display_unop(AstVisitor *v, AstNodeFull_UnOp unop_n) {
    FILE *stream = (FILE *)v->ctx;
    fprintf(stream, "%s ", _str_un_op(unop_n.op));
    ast_visit(v, unop_n.arg);
}

void display_binop(AstVisitor *v, AstNodeFull_BinOp binop_n) {
    FILE *stream = (FILE *)v->ctx;

    fprintf(stream, "(");
    ast_visit(v, binop_n.lhs);
    fprintf(stream, " %s ", _str_bin_op(binop_n.op));
    ast_visit(v, binop_n.rhs);
    fprintf(stream, ")");
}



void display_ast(const Ast ast, NodeIdx idx, StrPool strs, FILE *stream){
    AstVisitor visitor = (AstVisitor){
        ast,
        strs,
        (void *)stream,
        {0},
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
    };

    ast_visit(&visitor, idx);
    fprintf(stream, "\n");
}

