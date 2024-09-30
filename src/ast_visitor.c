#include <stdio.h>
#include <stdlib.h>
#include "../include/ast.h"
#include "../include/str_pool.h"
#include "../include/ast_visitor.h"

#include <inttypes.h>

// VISITOR
struct Visitor_S {
    Ast ast;
    StrPool strs;
    void *ctx;
    Location loc;

    /* Expression visitors */
    void (*visit_prog)(Visitor v, AstNodeFull_List prog_n);
    void (*visit_block)(Visitor v, AstNodeFull_List block_n);
    void (*visit_var_decl)(Visitor v, AstNodeFull_VarDecl var_decl_n);
    void (*visit_meth_decl)(Visitor v, AstNodeFull_MethDecl meth_decl_n);
    void (*visit_param)(Visitor v, Type type, StrIdx ident);
    void (*visit_asgn)(Visitor v, AstNodeFull_Asgn asgn_n);
    void (*visit_if)(Visitor v, AstNodeFull_If if_n);
    void (*visit_while)(Visitor v, AstNodeFull_While while_n);
    void (*visit_ret)(Visitor v, NodeIdx expr_idx);
    void (*visit_meth_call)(Visitor v, AstNodeFull_MethCall meth_call_n);
    void (*visit_var)(Visitor v, StrIdx ident);
    void (*visit_int_lit)(Visitor v, uint32_t val);
    void (*visit_bool_lit)(Visitor v, bool val);
    void (*visit_unop)(Visitor v, AstNodeFull_UnOp unop_n);
    void (*visit_binop)(Visitor v, AstNodeFull_BinOp binop_n);
};

Visitor init_visitor(
    Ast ast,
    StrPool strs,
    void *ctx,

    void (*visit_prog)(Visitor v, AstNodeFull_List prog_n),
    void (*visit_block)(Visitor v, AstNodeFull_List block_n),
    void (*visit_var_decl)(Visitor v, AstNodeFull_VarDecl var_decl_n),
    void (*visit_meth_decl)(Visitor v, AstNodeFull_MethDecl meth_decl_n),
    void (*visit_param)(Visitor v, Type type, StrIdx ident),
    void (*visit_asgn)(Visitor v, AstNodeFull_Asgn asgn_n),
    void (*visit_if)(Visitor v, AstNodeFull_If if_n),
    void (*visit_while)(Visitor v, AstNodeFull_While while_n),
    void (*visit_ret)(Visitor v, NodeIdx expr_idx),
    void (*visit_meth_call)(Visitor v, AstNodeFull_MethCall meth_call_n),
    void (*visit_var)(Visitor v, StrIdx ident),
    void (*visit_int_lit)(Visitor v, uint32_t val),
    void (*visit_bool_lit)(Visitor v, bool val),
    void (*visit_unop)(Visitor v, AstNodeFull_UnOp unop_n),
    void (*visit_binop)(Visitor v, AstNodeFull_BinOp binop_n)) {

    Visitor self = malloc(sizeof(*self));
    self->ast = ast;
    self->strs = strs;
    self->ctx = ctx;
    self->loc = (Location){ .line = 0, .col = 0 };

    self->visit_prog = visit_prog;
    self->visit_block = visit_block;
    self->visit_var_decl = visit_var_decl;
    self->visit_meth_decl = visit_meth_decl;
    self->visit_param = visit_param;
    self->visit_asgn = visit_asgn;
    self->visit_if = visit_if;
    self->visit_while = visit_while;
    self->visit_ret = visit_ret;
    self->visit_meth_call = visit_meth_call;
    self->visit_var = visit_var;
    self->visit_int_lit = visit_int_lit;
    self->visit_bool_lit = visit_bool_lit;
    self->visit_unop = visit_unop;
    self->visit_binop = visit_binop;

    return self;
}

void ast_visit(Visitor self, NodeIdx idx) {
    AstNode *node = &self->ast.nodes[idx];

    if (node == NULL) {
        return;
    }

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

}

void visitor_release(Visitor self) {
    free(self);
}

// getters
void *visitor_get_ctx(Visitor self) {
    return self->ctx;
}

Ast visitor_get_ast(Visitor self) {
    return self->ast;
}

StrPool visitor_get_strs(Visitor self) {
    return self->strs;
}

Location visitor_get_loc(Visitor self) {
    return self->loc;
}
