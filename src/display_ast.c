#include "ast.h"
#include "display.h"
#include "str_pool.h"
#include <stdio.h>

#define PPRINT(fmt, ...)                                                       \
    fprintf(                                                                   \
        pp->stream,                                                            \
        "%*s" fmt,                                                             \
        pp->do_indent ? pp->indent : (pp->do_indent = true, 1),                \
        "" __VA_OPT__(, ) __VA_ARGS__)

#define PP_LIST(lo, hi, action)                                                \
    do {                                                                       \
        if (lo == hi) {                                                        \
            PPRINT("[]\n");                                                    \
        } else {                                                               \
            PPRINT("[\n");                                                     \
            INDENT;                                                            \
            for (NodeIdx i = lo; i < hi; i++) {                                \
                action                                                         \
            }                                                                  \
            DEDENT;                                                            \
            PPRINT("]\n");                                                     \
        }                                                                      \
    } while (0)

// clang-format off
#define INDENT do { pp->indent += INDENT_SZ; } while (0)
#define DEDENT do { pp->indent -= INDENT_SZ; } while (0)
#define SKIP_INDENT do { pp->do_indent = false; } while (0)
// clang-format on

typedef struct Pp {
    StrPool strs;
    FILE *stream;
    uint32_t indent;
    bool do_indent;
} Pp;

static const char *op2str(AstNodeKind op) {
    switch (op) {
    case AstNodeKind_UNM: return "~";
    case AstNodeKind_NEG: return "!";
    case AstNodeKind_MUL: return "*";
    case AstNodeKind_DIV: return "/";
    case AstNodeKind_MOD: return "%";
    case AstNodeKind_ADD: return "+";
    case AstNodeKind_SUB: return "-";
    case AstNodeKind_LT: return "<";
    case AstNodeKind_GT: return ">";
    case AstNodeKind_EQ: return "==";
    case AstNodeKind_AND: return "&&";
    case AstNodeKind_OR: return "||";
    default: unreachable;
    }
}

const char *ty2str(Type type) {
    switch (type) {
    case Type_VOID: return "void";
    case Type_INT: return "integer";
    case Type_BOOL: return "bool";
    default: unreachable;
    }
}

static void display_expr(Pp *pp, Ast ast, NodeIdx idx);

static void display_meth_call(Pp *pp, Ast ast, NodeIdx idx) {
    AstNodeFull_MethCall call = Ast_full_meth_call(ast, idx);

    PPRINT("(meth_call:\n");
    INDENT;
    PPRINT("meth: %s\n", StrPool_get(&pp->strs, call.meth_ident));
    PPRINT("args:");
    SKIP_INDENT;

    PP_LIST(call.args_begin, call.args_end, display_expr(pp, ast, i););

    DEDENT;
    PPRINT(")\n");
}

static void display_expr(Pp *pp, Ast ast, NodeIdx idx) {
    AstNode *node = &ast.nodes[idx];

    switch (node->kind) {
    case AstNodeKind_METH_CALL:
        display_meth_call(pp, ast, idx);
        break;

    case AstNodeKind_VAR:
        PPRINT("%s\n", StrPool_get(&pp->strs, node->data.lhs));
        break;

    case AstNodeKind_INT_LIT:
        PPRINT("%d\n", node->data.lhs);
        break;

    case AstNodeKind_BOOL_LIT:
        PPRINT("%s\n", node->data.lhs ? "true" : "false");
        break;

    case AstNodeKind_UNM:
    case AstNodeKind_NEG:
        PPRINT("(%s:", op2str(node->kind));
        SKIP_INDENT;
        display_expr(pp, ast, node->data.lhs);
        PPRINT(")\n");
        break;

    case AstNodeKind_MUL:
    case AstNodeKind_DIV:
    case AstNodeKind_MOD:
    case AstNodeKind_ADD:
    case AstNodeKind_SUB:
    case AstNodeKind_LT:
    case AstNodeKind_GT:
    case AstNodeKind_EQ:
    case AstNodeKind_AND:
    case AstNodeKind_OR:
        PPRINT("(%s:\n", op2str(node->kind));
        INDENT;
        PPRINT("lhs:");
        SKIP_INDENT;
        display_expr(pp, ast, node->data.lhs);
        PPRINT("rhs:");
        SKIP_INDENT;
        display_expr(pp, ast, node->data.rhs);
        DEDENT;
        PPRINT(")\n");
        break;

    default:
        unreachable;
    }
}

static void display_var_decl(Pp *pp, Ast ast, NodeIdx idx) {
    AstNodeFull_VarDecl var_decl = Ast_full_var_decl(ast, idx);

    PPRINT("(var_decl:\n");
    INDENT;
    PPRINT("type: %s\n", ty2str(var_decl.type));
    PPRINT("var: %s\n", StrPool_get(&pp->strs, var_decl.ident));
    PPRINT("init_expr:");
    SKIP_INDENT;
    display_expr(pp, ast, var_decl.init_expr);
    DEDENT;
    PPRINT(")\n");
}

static void display_stmt(Pp *pp, Ast ast, NodeIdx idx) {
    AstNode *node = &ast.nodes[idx];

    switch (node->kind) {
    case AstNodeKind_BLOCK: {
        AstNodeFull_List block = Ast_full_block(ast, idx);

        PP_LIST(block.begin, block.end, display_stmt(pp, ast, i););
    } break;

    case AstNodeKind_VAR_DECL_INIT:
        display_var_decl(pp, ast, idx);
        break;

    case AstNodeKind_ASGN: {
        AstNodeFull_Asgn asgn = Ast_full_asgn(ast, idx);

        PPRINT("(asgn:\n");
        INDENT;
        PPRINT("target: %s\n", StrPool_get(&pp->strs, asgn.target));
        PPRINT("expr:");
        SKIP_INDENT;
        display_expr(pp, ast, asgn.expr);
        DEDENT;
        PPRINT(")\n");
    } break;

    case AstNodeKind_IF_SMP:
    case AstNodeKind_IF_ALT: {
        AstNodeFull_If iff = Ast_full_if(ast, idx);

        PPRINT("(if:\n");
        INDENT;
        PPRINT("cond:");
        SKIP_INDENT;
        display_expr(pp, ast, iff.cond);
        PPRINT("then:");
        SKIP_INDENT;
        display_stmt(pp, ast, iff.then_b);
        if (iff.else_b != NO_NODE) {
            PPRINT("else:");
            SKIP_INDENT;
            display_stmt(pp, ast, iff.else_b);
        }
        DEDENT;
        PPRINT(")\n");
    } break;

    case AstNodeKind_WHILE: {
        AstNodeFull_While whilee = Ast_full_while(ast, idx);
            
        PPRINT("(while:\n");
        INDENT;
        PPRINT("cond:");
        SKIP_INDENT;
        display_expr(pp, ast, whilee.cond);
        PPRINT("body:");
        SKIP_INDENT;
        display_stmt(pp, ast, whilee.body);
        DEDENT;
        PPRINT(")\n");
    } break;

    case AstNodeKind_RET: {
        NodeIdx ret_val = node->data.lhs;

        PPRINT("(return:");
        SKIP_INDENT;

        if (node->data.lhs != NO_NODE) {
            display_expr(pp, ast, ret_val);
            PPRINT(")\n");
        } else {
            PPRINT("<no value>)\n");
        }
    } break;

    case AstNodeKind_METH_CALL:
        display_meth_call(pp, ast, idx);
        break;

    default:
        unreachable;
    }
}

static void display_meth_decl(Pp *pp, Ast ast, NodeIdx idx) {
    AstNodeFull_MethDecl meth_decl = Ast_full_meth_decl(ast, idx);

    PPRINT("(meth_decl:\n");
    INDENT;

    PPRINT("meth_name: %s\n", StrPool_get(&pp->strs, meth_decl.ident));
    PPRINT("ret_type: %s\n", ty2str(meth_decl.ret_type));
    PPRINT("params:");
    SKIP_INDENT;

    AstNodeFull_List params = Ast_full_list(ast, meth_decl.params);
    PP_LIST(params.begin, params.end, {
        AstNode *param = &ast.nodes[i];

        PPRINT("(param:\n");
        INDENT;
        PPRINT("type: %s\n", ty2str(param->data.lhs));
        PPRINT("ident: %s\n", StrPool_get(&pp->strs, param->data.rhs));
        DEDENT;
        PPRINT(")\n");
    });

    PPRINT("body:");
    SKIP_INDENT;

    if (meth_decl.body != NO_NODE) {
        display_stmt(pp, ast, meth_decl.body);
    } else {
        PPRINT("extern\n");
    }

    DEDENT;
    PPRINT(")\n");
}

static void display_prog(Pp *pp, Ast ast) {
    AstNodeFull_List prog = Ast_full_prog(ast);

    PPRINT("(program:");
    SKIP_INDENT;

    PP_LIST(prog.begin, prog.end, {
        AstNode *node = &ast.nodes[i];
        switch (node->kind) {
        case AstNodeKind_VAR_DECL_INIT:
            display_var_decl(pp, ast, i);
            break;

        case AstNodeKind_METH_DECL_IMPL:
        case AstNodeKind_METH_DECL:
            display_meth_decl(pp, ast, i);
            break;

        default:
            unreachable;
        }
    });

    PPRINT(")\n");
}

void display_ast(const Ast ast, StrPool strs, uint32_t indent, FILE *stream) {
    Pp pp = { .strs = strs, .stream = stream, .indent = indent, .do_indent = true };

    display_prog(&pp, ast);
}
