#include "consteval.h"

#include <stdint.h>

#include "ast.h"
#include "common.h"
#include "ir.h"

static int32_t getglbl(const GlobalVec *globals, StrIdx name) {
    for (size_t i = 0; i < globals->len; ++i) {
        const Global *globl = &globals->elems[i];

        if (globl->name == name) {
            return globl->value;
        }
    }

    unreachable;
}

static int32_t
eval_unary(Ast ast, AstNodeKind op, NodeIdx expr, const GlobalVec *globals) {
    int32_t eval_res = consteval(ast, expr, globals);

    switch (op) {
    case AstNodeKind_UNM:
        eval_res = -eval_res;
        break;

    case AstNodeKind_NEG:
        eval_res = !eval_res;
        break;

    default:
        unreachable;
    }

    return eval_res;
}

static int32_t eval_binary(
    Ast ast,
    AstNodeKind op,
    NodeIdx lhs_id,
    NodeIdx rhs_id,
    const GlobalVec *globals) {

    int32_t lhs = consteval(ast, lhs_id, globals);
    int32_t rhs = consteval(ast, rhs_id, globals);
    int32_t eval_res;

    switch (op) {
    case AstNodeKind_MUL:
        eval_res = lhs * rhs;
        break;

    case AstNodeKind_DIV:
        eval_res = lhs / rhs;
        break;

    case AstNodeKind_MOD:
        eval_res = lhs % rhs;
        break;

    case AstNodeKind_ADD:
        eval_res = lhs + rhs;
        break;

    case AstNodeKind_SUB:
        eval_res = lhs - rhs;
        break;

    case AstNodeKind_LT:
        eval_res = lhs < rhs;
        break;

    case AstNodeKind_GT:
        eval_res = lhs > rhs;
        break;

    case AstNodeKind_EQ:
        eval_res = lhs == rhs;
        break;

    case AstNodeKind_AND:
        eval_res = lhs && rhs;
        break;

    case AstNodeKind_OR:
        eval_res = lhs || rhs;
        break;

    default:
        unreachable;
    }

    return eval_res;
}

int32_t consteval(Ast ast, NodeIdx i, const GlobalVec *globals) {
    AstNode *node = &ast.nodes[i];

    int32_t eval_res;

    switch (node->kind) {
    case AstNodeKind_VAR:
        eval_res = getglbl(globals, node->data.lhs);
        break;

    case AstNodeKind_INT_LIT:
    case AstNodeKind_BOOL_LIT:
        eval_res = node->data.lhs;
        break;

    case AstNodeKind_UNM:
    case AstNodeKind_NEG:
        eval_res = eval_unary(ast, node->kind, node->data.lhs, globals);
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
        eval_res = eval_binary(
            ast, node->kind, node->data.lhs, node->data.rhs, globals);
        break;

    default:
        unreachable;
    }

    return eval_res;
}
