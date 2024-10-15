#include <stdio.h>
#include <stdlib.h>

#include "ast_visitor.h"

void ast_visit(AstVisitor *self, NodeIdx idx) {
    AstNode *node = &self->ast.nodes[idx];

    if (node == NULL) {
        return;
    }

    Location prev_loc = self->loc;
    self->loc = node->loc;

    switch (node->kind) {
    case AstNodeKind_PROG:
        self->visit_prog(self, Ast_full_prog(self->ast));
        break;

    case AstNodeKind_BLOCK:
        self->visit_block(self, Ast_full_block(self->ast, idx));
        break;

    case AstNodeKind_LIST:
        fprintf(stderr, "Visitor shouldn't be called directly with a list node");
        exit(EXIT_FAILURE);

    case AstNodeKind_VAR_DECL_INIT:
        self->visit_var_decl(self, Ast_full_var_decl(self->ast, idx));
        break;

    case AstNodeKind_VAR_DECL:
        fprintf(stderr, "Visitor shouldn't be called directly with a var decl");
        exit(EXIT_FAILURE);

    case AstNodeKind_METH_DECL_IMPL:
    case AstNodeKind_METH_DECL:
        self->visit_meth_decl(self, Ast_full_meth_decl(self->ast, idx));
        break;

    case AstNodeKind_METH_PROTO:
        fprintf(stderr, "Visitor shouldn't be called directly with a meth proto");
        exit(EXIT_FAILURE);

    case AstNodeKind_PARAM:
        self->visit_param(self, node->data.lhs, node->data.rhs);
        break;

    case AstNodeKind_ASGN:
        self->visit_asgn(self, Ast_full_asgn(self->ast, idx));
        break;

    case AstNodeKind_IF_SMP:
        self->visit_if(self, Ast_full_if(self->ast, idx));
        break;

    case AstNodeKind_IF_ALT:
        self->visit_if(self, Ast_full_if(self->ast, idx));
        break;

    case AstNodeKind_WHILE:
        self->visit_while(self, Ast_full_while(self->ast, idx));
        break;

    case AstNodeKind_RET:
        self->visit_ret(self, node->data.lhs);
        break;

    case AstNodeKind_METH_CALL:
        self->visit_meth_call(self, Ast_full_meth_call(self->ast, idx));
        break;

    case AstNodeKind_VAR:
        self->visit_var(self, node->data.lhs);
        break;

    case AstNodeKind_INT_LIT:
        self->visit_int_lit(self, node->data.lhs);
        break;

    case AstNodeKind_BOOL_LIT:
        self->visit_bool_lit(self, node->data.lhs);
        break;

    case AstNodeKind_UNM:
    case AstNodeKind_NEG:
        self->visit_unop(self, Ast_full_unop(self->ast, idx));
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
        self->visit_binop(self, Ast_full_binop(self->ast, idx));
        break;

    }

    self->loc = prev_loc;
}

