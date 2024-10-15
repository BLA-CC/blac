#ifndef AST_VISITOR_H
#define AST_VISITOR_H

#include "ast.h"
#include "str_pool.h"

typedef struct Visitor_S {
    Ast ast;
    StrPool strs;
    void *ctx;
    Location loc;

    /* Expression visitors */
    void (*visit_prog)(struct Visitor_S *v, AstNodeFull_List prog_n);
    void (*visit_block)(struct Visitor_S *v, AstNodeFull_List block_n);
    void (*visit_var_decl)(struct Visitor_S *v, AstNodeFull_VarDecl var_decl_n);
    void (*visit_meth_decl)(struct Visitor_S *v, AstNodeFull_MethDecl meth_decl_n);
    void (*visit_param)(struct Visitor_S *v, Type type, StrIdx ident);
    void (*visit_asgn)(struct Visitor_S *v, AstNodeFull_Asgn asgn_n);
    void (*visit_if)(struct Visitor_S *v, AstNodeFull_If if_n);
    void (*visit_while)(struct Visitor_S *v, AstNodeFull_While while_n);
    void (*visit_ret)(struct Visitor_S *v, NodeIdx expr_idx);
    void (*visit_meth_call)(struct Visitor_S *v, AstNodeFull_MethCall meth_call_n);
    void (*visit_var)(struct Visitor_S *v, StrIdx ident);
    void (*visit_int_lit)(struct Visitor_S *v, uint32_t val);
    void (*visit_bool_lit)(struct Visitor_S *v, bool val);
    void (*visit_unop)(struct Visitor_S *v, AstNodeFull_UnOp unop_n);
    void (*visit_binop)(struct Visitor_S *v, AstNodeFull_BinOp binop_n);
} Visitor;

/**
 * @brief Traverses the AST from the given root node, visiting expressions
 * and statements based on the visitor callbacks. It will print errors in stderr
 * if there is any.
 *
 * @param[in] self The visitor responsible for traversing the AST.
 * @param[in] node_id The ID of the root node in the AST to begin traversal.
 *
 */
void ast_visit(Visitor *self, NodeIdx idx);

#endif // AST_VISITOR_H

