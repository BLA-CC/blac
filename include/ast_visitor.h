#ifndef AST_VISITOR_H
#define AST_VISITOR_H

#include "ast.h"
#include "str_pool.h"

typedef struct Visitor_S *Visitor;

/**
 * @brief Initializes a visitor to traverse an Abstract Syntax Tree (AST).
 *
 * This function sets up a Visitor structure with user-defined callback
 * functions for handling various expression and statement types.
 *
 * @param[in] ast              The Abstract Syntax Tree to traverse.
 * @param[in] strs             String pool for managing identifier strings.
 * @param[in] ctx  User-provided arguments for customized traversal.
 * @param[in] visit_<node>     Callback function for visiting <node>
 * @return A newly initialized Vis  itor, or NULL if initialization fails.
 */
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
    void (*visit_binop)(Visitor v, AstNodeFull_BinOp binop_n)
);

/**
 * @brief Traverses the AST from the given root node, visiting expressions
 * and statements based on the visitor callbacks. It will print errors in stderr
 * if there is any.
 *
 * @param[in] self The visitor responsible for traversing the AST.
 * @param[in] node_id The ID of the root node in the AST to begin traversal.
 *
 */
void ast_visit(Visitor self, NodeIdx idx);


/**
 * @brief Releases the memory associated with a visitor.
 *
 * This function frees all memory allocated by the visitor, including any
 * internal data structures or additional arguments.
 *
 * @param[in] self The visitor to release.
 */
void visitor_release(Visitor self);

/**
 * @brief Retrieves the additional arguments passed during visitor
 * initialization.
 *
 * @param[in] self The visitor structure.
 *
 * @return Pointer to the user-provided additional arguments.
 */
void *visitor_get_ctx(Visitor self);

/**
 * @brief Retrieves the AST associated with the visitor.
 *
 * @param[in] self The visitor structure.
 *
 * @return The AST being traversed.
 */
Ast visitor_get_ast(Visitor self);

/**
 * @brief Retrieves the string pool associated with the visitor.
 *
 * @param[in] self The visitor structure.
 *
 * @return The string pool used for managing identifiers.
 */
StrPool visitor_get_strs(Visitor self);

Location visitor_get_loc(Visitor self);

#endif // AST_VISITOR_H