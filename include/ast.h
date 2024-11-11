#ifndef _AST_H
#define _AST_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "common.h"
#include "str_pool.h"
#include "vec.h"

/*
 * Location in source code of a node.
 * Used mostly to report errors.
 * */
typedef struct {
    uint32_t line;
    uint32_t col;
} Location;

/**
 * This data type dictates how `AstNode`s should be interpreted
 * (See `AstNode`)
 */
typedef enum {
    /**
     * `sublist[lhs..rhs]`.
     * Nodes in this list must be one of
     * VAR_DECL, VAR_DECL_INIT, METH_DECL or METH_DECL_IMPL.
     */
    AstNodeKind_PROG,
    /**
     * sublist[lhs..rhs].
     * Nodes in this list must be one of
     * VAR_DECL, VAR_DECL_INIT, ASGN, IF_SMP, IF_ALT, WHILE, RET or METH_CALL.
     */
    AstNodeKind_BLOCK,
    /**
     * sublist[lhs..rhs].
     * Generic, type of nodes depends on how it is used.
     * TODO: delete in favour of typed lists.
     */
    AstNodeKind_LIST,
    /**
     * `lhs` must be a VAR_DECL, `rhs` must be an expression.
     * A variable declared in `lhs` is initialized to `rhs`.
     */
    AstNodeKind_VAR_DECL_INIT,
    /**
     * `lhs` is a Type, `rhs` is a StrIdx.
     * Variable `rhs` is declared with type `lhs`.
     */
    AstNodeKind_VAR_DECL,
    /**
     * `lhs` must be a METH_DECL, `rhs` must be a BLOCK.
     * A method declared in `lhs` with body `rhs`.
     */
    AstNodeKind_METH_DECL_IMPL,
    /**
     * `lhs` is a StrIdx, `rhs` must be a METH_PROTO.
     * Method `lhs` is declared with prototype `rhs`.
     */
    AstNodeKind_METH_DECL,
    /**
     * `lhs` must be a LIST, `rhs` is a Type.
     * Elements in `lhs` must be PARAMs
     * The prototype of a method with parameters `lhs` and return type `rhs`.
     */
    AstNodeKind_METH_PROTO,
    /**
     * `lhs` is a Type, `rhs` is a StrIdx.
     * A parameter with type `lhs`, named `rhs`.
     */
    AstNodeKind_PARAM,
    /**
     * `lhs` is a StrIdx, `rhs` must be an expression.
     * `lhs = rhs`.
     */
    AstNodeKind_ASGN,
    /**
     * `lhs` must be an expression, `rhs` must be a BLOCK.
     * `if (lhs) then rhs`.
     */
    AstNodeKind_IF_SMP,
    /**
     * `lhs` must be an expression, `rhs` AND `rhs + 1` must be BLOCKs.
     * `if (lhs) then rhs else <rhs + 1>`.
     */
    AstNodeKind_IF_ALT,
    /**
     * `lhs` must be an expression, `rhs` must be a BLOCK.
     * `while (lhs) rhs`.
     */
    AstNodeKind_WHILE,
    /**
     * `lhs` must be an expression or NO_NODE, `rhs` is unused.
     * `return lhs`.
     */
    AstNodeKind_RET,
    /**
     * `lhs` is a StrIdx, `rhs` must be a LIST.
     * Elements in `rhs` must be expressions.
     * `lhs(rhs)`
     */
    AstNodeKind_METH_CALL,
    /**
     * `lhs` is a StrIdx, `rhs` is unused.
     */
    AstNodeKind_VAR,
    /**
     * `lhs` holds the literal.
     */
    AstNodeKind_INT_LIT,
    /**
     * `lhs` holds the literal.
     */
    AstNodeKind_BOOL_LIT,
    /**
     * `lhs` must be an expression.
     * `-lhs`
     */
    AstNodeKind_UNM,
    /**
     * `lhs` must be an expression.
     * `!lhs`
     */
    AstNodeKind_NEG,
    /**
     * `lhs` and `rhs` must be expressions.
     * `lhs * rhs`
     */
    AstNodeKind_MUL,
    /**
     * `lhs` and `rhs` must be expressions.
     * `lhs / rhs`
     */
    AstNodeKind_DIV,
    /**
     * `lhs` and `rhs` must be expressions.
     * `lhs % rhs`
     */
    AstNodeKind_MOD,
    /**
     * `lhs` and `rhs` must be expressions.
     * `lhs + rhs`
     */
    AstNodeKind_ADD,
    /**
     * `lhs` and `rhs` must be expressions.
     * `lhs - rhs`
     */
    AstNodeKind_SUB,
    /**
     * `lhs` and `rhs` must be expressions.
     * `lhs < rhs`
     */
    AstNodeKind_LT,
    /**
     * `lhs` and `rhs` must be expressions.
     * `lhs > rhs`
     */
    AstNodeKind_GT,
    /**
     * `lhs` and `rhs` must be expressions.
     * `lhs == rhs`
     */
    AstNodeKind_EQ,
    /**
     * `lhs` and `rhs` must be expressions.
     * `lhs && rhs`
     */
    AstNodeKind_AND,
    /**
     * `lhs` and `rhs` must be expressions.
     * `lhs || rhs`
     */
    AstNodeKind_OR,
} AstNodeKind;

/*
 * The Abstract Syntax Tree is stored as an array of nodes.
 * References between nodes are stored as 32 bit integers.
 * Since the root of the AST is always stored at index 0
 * and no nodes can hold a reference to the root,
 * 0 is available to indicate null.
 * */
typedef struct {
    struct AstNode *nodes;
    uint32_t len;
} Ast;

void Ast_release(Ast *ast);

typedef uint32_t NodeIdx;
#define NO_NODE  0
#define AST_ROOT 0

/*
 * The `data` of a node is to be interpreted according to its `kind`.
 * Each of `lhs`, `rhs` could either represent a pointer to another
 * node or a literal value.
 * */
typedef struct AstNode {
    Location loc;
    AstNodeKind kind;
    struct {
        NodeIdx lhs, rhs;
    } data;
} AstNode;

#define AstNode_mk(l, k, ds...)                                                \
    (AstNode) {                                                                \
        .loc = l, .kind = AstNodeKind_##k, .data = { ds }                      \
    }

Vec_Proto(AstNode);

// To enforce (some) type safety, an API is defined to retrieve information
// from the AST without unadvertently casting the nodes.

typedef struct {
    NodeIdx begin;
    NodeIdx end;
} AstNodeFull_List;

typedef struct {
    Type type;
    StrIdx ident;
    NodeIdx init_expr;
} AstNodeFull_VarDecl;

typedef struct {
    Type ret_type;
    StrIdx ident;
    NodeIdx params;
    NodeIdx body;
} AstNodeFull_MethDecl;

typedef struct {
    StrIdx target;
    NodeIdx expr;
} AstNodeFull_Asgn;

typedef struct {
    NodeIdx cond;
    NodeIdx then_b;
    NodeIdx else_b;
} AstNodeFull_If;

typedef struct {
    NodeIdx cond;
    NodeIdx body;
} AstNodeFull_While;

typedef struct {
    StrIdx meth_ident;
    NodeIdx args_begin;
    NodeIdx args_end;
} AstNodeFull_MethCall;

AstNodeFull_List Ast_full_prog(Ast ast);

AstNodeFull_List Ast_full_block(Ast ast, NodeIdx idx);

AstNodeFull_List Ast_full_list(Ast ast, NodeIdx idx);

AstNodeFull_VarDecl Ast_full_var_decl(Ast ast, NodeIdx idx);

AstNodeFull_MethDecl Ast_full_meth_decl(Ast ast, NodeIdx idx);

AstNodeFull_Asgn Ast_full_asgn(Ast ast, NodeIdx idx);

AstNodeFull_If Ast_full_if(Ast ast, NodeIdx idx);

AstNodeFull_While Ast_full_while(Ast ast, NodeIdx idx);

AstNodeFull_MethCall Ast_full_meth_call(Ast ast, NodeIdx idx);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _AST_H */
