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
        eval_res = -consteval(ast, node->data.lhs, globals);
        break;

    case AstNodeKind_NEG:
        eval_res = !consteval(ast, node->data.lhs, globals);
        break;

    case AstNodeKind_MUL: {
        int32_t lhs = consteval(ast, node->data.lhs, globals);
        int32_t rhs = consteval(ast, node->data.rhs, globals);
        eval_res = lhs * rhs;
    } break;

    case AstNodeKind_DIV: {
        int32_t lhs = consteval(ast, node->data.lhs, globals);
        int32_t rhs = consteval(ast, node->data.rhs, globals);
        eval_res = lhs / rhs;
    } break;

    case AstNodeKind_MOD: {
        int32_t lhs = consteval(ast, node->data.lhs, globals);
        int32_t rhs = consteval(ast, node->data.rhs, globals);
        eval_res = lhs % rhs;
    } break;

    case AstNodeKind_ADD: {
        int32_t lhs = consteval(ast, node->data.lhs, globals);
        int32_t rhs = consteval(ast, node->data.rhs, globals);
        eval_res = lhs + rhs;
    } break;

    case AstNodeKind_SUB: {
        int32_t lhs = consteval(ast, node->data.lhs, globals);
        int32_t rhs = consteval(ast, node->data.rhs, globals);
        eval_res = lhs - rhs;
    } break;

    case AstNodeKind_LT: {
        int32_t lhs = consteval(ast, node->data.lhs, globals);
        int32_t rhs = consteval(ast, node->data.rhs, globals);
        eval_res = lhs < rhs;
    } break;

    case AstNodeKind_GT: {
        int32_t lhs = consteval(ast, node->data.lhs, globals);
        int32_t rhs = consteval(ast, node->data.rhs, globals);
        eval_res = lhs > rhs;
    } break;

    case AstNodeKind_EQ: {
        int32_t lhs = consteval(ast, node->data.lhs, globals);
        int32_t rhs = consteval(ast, node->data.rhs, globals);
        eval_res = lhs * rhs;
    } break;

    case AstNodeKind_AND: {
        int32_t lhs = consteval(ast, node->data.lhs, globals);
        eval_res = lhs && consteval(ast, node->data.rhs, globals);
    } break;

    case AstNodeKind_OR: {
        int32_t lhs = consteval(ast, node->data.lhs, globals);
        eval_res = lhs || consteval(ast, node->data.rhs, globals);
    } break;

    default:
        unreachable;
    }

    return eval_res;
}
