#ifndef AST_VISITOR_H
#define AST_VISITOR_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "ast.h"
#include "str_pool.h"

typedef struct AstVisitor {
    Ast ast;
    StrPool strs;
    void *ctx;
    Location loc;

    /* Expression visitors */
    // clang-format off
    void (*visit_prog)(struct AstVisitor *v, AstNodeFull_List prog_n);
    void (*visit_block)(struct AstVisitor *v, AstNodeFull_List block_n);
    void (*visit_var_decl)(struct AstVisitor *v, AstNodeFull_VarDecl var_decl_n);
    void (*visit_meth_decl)(struct AstVisitor *v, AstNodeFull_MethDecl meth_decl_n);
    void (*visit_param)(struct AstVisitor *v, Type type, StrIdx ident);
    void (*visit_asgn)(struct AstVisitor *v, AstNodeFull_Asgn asgn_n);
    void (*visit_if)(struct AstVisitor *v, AstNodeFull_If if_n);
    void (*visit_while)(struct AstVisitor *v, AstNodeFull_While while_n);
    void (*visit_ret)(struct AstVisitor *v, NodeIdx expr_idx);
    void (*visit_meth_call)(struct AstVisitor *v, AstNodeFull_MethCall meth_call_n);
    void (*visit_var)(struct AstVisitor *v, StrIdx ident);
    void (*visit_int_lit)(struct AstVisitor *v, uint32_t val);
    void (*visit_bool_lit)(struct AstVisitor *v, bool val);
    void (*visit_unop)(struct AstVisitor *v, AstNodeFull_UnOp unop_n);
    void (*visit_binop)(struct AstVisitor *v, AstNodeFull_BinOp binop_n);
    // clang-format on
} AstVisitor;

/**
 * @brief Traverses the AST from the given root node, visiting expressions
 * and statements based on the visitor callbacks. It will print errors in stderr
 * if there is any.
 *
 * @param[in] self The visitor responsible for traversing the AST.
 * @param[in] node_id The ID of the root node in the AST to begin traversal.
 *
 */
void ast_visit(AstVisitor *self, NodeIdx idx);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif // AST_VISITOR_H
