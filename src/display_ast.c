#include "ast.h"
#include "ast_visitor.h"
#include "display.h"
#include "str_pool.h"
#include <stdio.h>

#define INDENT_SZ 2
#define PPRINT(fmt, ...)                                                       \
    fprintf(                                                                   \
        ctx->stream,                                                           \
        "%*s" fmt,                                                             \
        ctx->skip_indent ? (ctx->skip_indent = false, 1) : ctx->indent,        \
            "" __VA_OPT__(,)                                                   \
            __VA_ARGS__)
#define INDENT do { ctx->indent += INDENT_SZ; } while (0)
#define DEDENT do { ctx->indent -= INDENT_SZ; } while (0)
#define SKIP_INDENT do { ctx->skip_indent = true; } while (0)

typedef struct PrintCtx {
    FILE *stream;
    uint32_t indent;
    bool skip_indent;
} PrintCtx;

static const char *_str_un_op(UnOp op) {
    switch (op) {
    case UnOp_NEG:
        return "!";
    case UnOp_UNM:
        return "~";
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
        return "INVALID";
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
    PrintCtx *ctx = (PrintCtx *)v->ctx;

    if (begin == end) {
        PPRINT("[]\n");
    } else {
        PPRINT("[\n");
        INDENT;
        for (NodeIdx i = begin; i < end; i++) {
            ast_visit(v, i);
        }
        DEDENT;
        PPRINT("]\n");
    }
}

void display_prog(AstVisitor *v, AstNodeFull_List prog_n) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;

    PPRINT("(program:");
    SKIP_INDENT;
    display_list(v, prog_n.begin, prog_n.end);
    PPRINT(")\n");
}

void display_block(AstVisitor *v, AstNodeFull_List block_n) {
    display_list(v, block_n.begin, block_n.end);
}

void display_var_decl(AstVisitor *v, AstNodeFull_VarDecl var_decl_n) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;
    StrPool strs = v->strs;

    PPRINT("(var_decl:\n");
    INDENT;
    PPRINT("type: %s\n", _str_type(var_decl_n.type));
    PPRINT("var: %s\n", StrPool_get(&strs, var_decl_n.ident));
    if (var_decl_n.init_expr != NO_NODE) {
        PPRINT("init_expr:");
        SKIP_INDENT;
        ast_visit(v, var_decl_n.init_expr);
    }
    DEDENT;
    PPRINT(")\n");
}

void display_meth_decl(AstVisitor *v, AstNodeFull_MethDecl meth_decl_n) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;
    StrPool strs = v->strs;

    PPRINT("(meth_decl:\n");
    INDENT;
    PPRINT("meth_name: %s\n", StrPool_get(&strs, meth_decl_n.ident));
    PPRINT("ret_type: %s\n", _str_type(meth_decl_n.ret_type));
    PPRINT("params:");
    AstNodeFull_List params = Ast_full_list(v->ast, meth_decl_n.params);
    SKIP_INDENT;
    display_list(v, params.begin, params.end);
    PPRINT("body:");

    SKIP_INDENT;
    if (meth_decl_n.body != NO_NODE) {
        ast_visit(v, meth_decl_n.body);
    } else {
        PPRINT("extern\n");
    }

    DEDENT;
    PPRINT(")\n");
}

void display_param(AstVisitor *v, Type type, StrIdx ident) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;
    StrPool strs = v->strs;

    PPRINT("(param:\n");
    INDENT;
    PPRINT("type: %s\n", _str_type(type));
    PPRINT("ident: %s\n", StrPool_get(&strs, ident));
    DEDENT;
    PPRINT(")\n");
}


void display_if(AstVisitor *v, AstNodeFull_If if_n) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;

    PPRINT("(if:\n");
    INDENT;
    PPRINT("cond:");
    SKIP_INDENT;
    ast_visit(v, if_n.cond);
    PPRINT("then:");
    SKIP_INDENT;
    ast_visit(v, if_n.then_b);
    if (if_n.else_b != NO_NODE) {
        PPRINT("else:");
        SKIP_INDENT;
        ast_visit(v, if_n.else_b);
    }
    DEDENT;
    PPRINT(")\n");
}

void display_while(AstVisitor *v, AstNodeFull_While while_n) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;

    PPRINT("(while:\n");
    INDENT;
    PPRINT("cond:");
    SKIP_INDENT;
    ast_visit(v, while_n.cond);
    PPRINT("body:");
    SKIP_INDENT;
    ast_visit(v, while_n.body);
    DEDENT;
    PPRINT(")\n");
}

void display_ret(AstVisitor *v, NodeIdx expr_idx) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;

    PPRINT("(return:");
    SKIP_INDENT;

    if (expr_idx != NO_NODE) {
        ast_visit(v, expr_idx);
        PPRINT(")\n");
    } else {
        PPRINT("<no value>)\n");
    }
}


void display_meth_call(AstVisitor *v, AstNodeFull_MethCall meth_call_n) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;
    StrPool strs = v->strs;

    PPRINT("(meth_call:\n");
    INDENT;
    PPRINT("meth: %s\n", StrPool_get(&strs, meth_call_n.meth_ident));
    PPRINT("args:");
    SKIP_INDENT;
    display_list(v, meth_call_n.args_begin, meth_call_n.args_end);
    DEDENT;
    PPRINT(")\n");
}

void display_var(AstVisitor *v, StrIdx ident) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;
    StrPool strs = v->strs;

    PPRINT("%s\n", StrPool_get(&strs, ident));
}

void display_int_lit(AstVisitor *v, uint32_t val) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;

    PPRINT("%d\n", val);
}

void display_bool_lit(AstVisitor *v, bool val) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;

    PPRINT("%s\n", _str_bool(val));
}

void display_asgn(AstVisitor *v, AstNodeFull_Asgn asgn_n) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;
    StrPool strs = v->strs;

    PPRINT("(asgn:\n");
    INDENT;
    PPRINT("target: %s\n" , StrPool_get(&strs, asgn_n.target));
    PPRINT("expr:");
    SKIP_INDENT;
    ast_visit(v, asgn_n.expr);
    DEDENT;
    PPRINT(")\n");
}


void display_unop(AstVisitor *v, AstNodeFull_UnOp unop_n) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;

    PPRINT("(%s:", _str_un_op(unop_n.op));
    SKIP_INDENT;
    ast_visit(v, unop_n.arg);
    PPRINT(")\n");
}

void display_binop(AstVisitor *v, AstNodeFull_BinOp binop_n) {
    PrintCtx *ctx = (PrintCtx *)v->ctx;

    PPRINT("(%s:\n", _str_bin_op(binop_n.op));
    INDENT;
    PPRINT("lhs:");
    SKIP_INDENT;
    ast_visit(v, binop_n.lhs);
    PPRINT("rhs:");
    SKIP_INDENT;
    ast_visit(v, binop_n.rhs);
    DEDENT;
    PPRINT(")\n");
}



void display_ast(const Ast ast, StrPool strs, NodeIdx indent, FILE *stream){
    PrintCtx ctx = { .stream = stream, .indent = indent };

    AstVisitor visitor = (AstVisitor){
        ast,
        strs,
        &ctx,
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

    ast_visit(&visitor, AST_ROOT);
}

