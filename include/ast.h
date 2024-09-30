#ifndef _AST_H
#define _AST_H

#include "common.h"
#include "str_pool.h"
#include "vec.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef struct {
    uint32_t line;
    uint32_t col;
} Location;

typedef uint32_t NodeIdx;
#define NO_NODE 0

/* 
 * PROG: the root of the ast
 *  vars: ptr? to a LIST of VAR_DECLs
 *  meths: ptr? to a LIST of METH_DECLs
 * VAR_DECL:
 *  type: the declared type
 *  ident: the name of the variable
 *  expr: a ptr to the initializer expression
 * METH_DECL: the declaration of a method
 *  ident: the name of the method
 *  proto: a ptr to the method's prototype
 *  body: a ptr to a block, or NO_NODE if this is an `extern` declaration
 * METH_PROTO: a method's prototype
 *  params: a ptr? to a LIST of PARAMs
 *  ret_type: the return type of the method
 * PARAM: a (formal) parameter
 *  type: the parameter's type
 *  ident: the parameter's name
 * ASGN:
 *  var_ident: the affected variable's name
 *  expr: a ptr to the rhs of the assigned
 * IF:
 *  cond: a ptr to the condition expression
 *  then_b: a ptr to a BLOCK
 *  else_b: a ptr? to a BLOCK, NO_NODE if there is no else branch
 * WHILE:
 *  cond: a ptr to the condition expression
 *  body: a ptr to a BLOCK
 * RET:
 *  expr: a ptr? to an expression, NO_NODE if no value is returned
 * BLOCK:
 *  vars: a ptr? to a LIST of VAR_DECLs
 *  stmts: a ptr? to a LIST of statements
 * NOP:
 *  useful only to parse empty statements
 * METH_CALL:
 *  meth_ident: the method's name
 *  args: a ptr? to a LIST of expressions
 * VAR: obvious
 * INT_LIT: obvious
 * BOOL_LIT: obvious
 * UNOP: <op> arg
 * BINOP: lhs <op> rhs
 * LIST:
 *      lists are stored as a pair [begin, end] of ptrs:
 *          begin points to the first element
 *          end points to the element immediately after the last one
 *      every ptr to a LIST is always optional (ptr?): a list of length
 *          zero is represented as NO_NODE
 * */

typedef enum {
    AstNodeKind_PROG,
    AstNodeKind_BLOCK,
    AstNodeKind_LIST,
    AstNodeKind_VAR_DECL_INIT,
    AstNodeKind_VAR_DECL,
    AstNodeKind_METH_DECL_IMPL,
    AstNodeKind_METH_DECL,
    AstNodeKind_METH_PROTO,
    AstNodeKind_PARAM,
    AstNodeKind_ASGN,
    AstNodeKind_IF_SMP,
    AstNodeKind_IF_ALT,
    AstNodeKind_WHILE,
    AstNodeKind_RET,
    AstNodeKind_METH_CALL,
    AstNodeKind_VAR,
    AstNodeKind_INT_LIT,
    AstNodeKind_BOOL_LIT,
    AstNodeKind_UNM,
    AstNodeKind_NEG,
    AstNodeKind_MUL,
    AstNodeKind_DIV,
    AstNodeKind_MOD,
    AstNodeKind_ADD,
    AstNodeKind_SUB,
    AstNodeKind_LT,
    AstNodeKind_GT,
    AstNodeKind_EQ,
    AstNodeKind_AND,
    AstNodeKind_OR,
} AstNodeKind;

typedef struct {
    Location loc;
    AstNodeKind kind;
    struct { NodeIdx lhs, rhs; } data;
} AstNode;

#define AstNode_mk(l, k, ds...)     \
    (AstNode){                      \
        .loc = l,                   \
        .kind = AstNodeKind_ ## k,  \
        .data = { ds }              \
    }

typedef struct {
    AstNode *nodes;
    uint32_t len;
} Ast;

Vec_Proto(AstNode);

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
    NodeIdx params_begin;
    NodeIdx params_end;
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

typedef enum {
    UnOp_UNM = AstNodeKind_UNM,
    UnOp_NEG,
} UnOp;

typedef struct {
    NodeIdx arg;
    UnOp op;
} AstNodeFull_UnOp;

typedef enum {
    BinOp_MUL = AstNodeKind_MUL,
    BinOp_DIV,
    BinOp_MOD,
    BinOp_ADD,
    BinOp_SUB,
    BinOp_LT,
    BinOp_GT,
    BinOp_EQ,
    BinOp_AND,
    BinOp_OR,
} BinOp;

typedef struct {
    NodeIdx lhs;
    NodeIdx rhs;
    BinOp op;
} AstNodeFull_BinOp;

AstNodeFull_List Ast_full_prog(Ast ast);

AstNodeFull_List Ast_full_block(Ast ast, NodeIdx idx);

AstNodeFull_VarDecl Ast_full_var_decl(Ast ast, NodeIdx idx);

AstNodeFull_MethDecl Ast_full_meth_decl(Ast ast, NodeIdx idx);

AstNodeFull_Asgn Ast_full_asgn(Ast ast, NodeIdx idx);

AstNodeFull_If Ast_full_if(Ast ast, NodeIdx idx);

AstNodeFull_While Ast_full_while(Ast ast, NodeIdx idx);

AstNodeFull_MethCall Ast_full_meth_call(Ast ast, NodeIdx idx);

AstNodeFull_UnOp Ast_full_unop(Ast ast, NodeIdx idx);

AstNodeFull_BinOp Ast_full_binop(Ast ast, NodeIdx idx);

#define AST_LIST_FOREACH(ast, begin, end, var, action)  \
    do {                                                \
        for (uint32_t _idx_ ## var = begin;             \
            _idx_ ## var < end;                         \
            _idx_ ## var++) {                           \
            AstNode *var = &ast[_idx_ ## var];          \
            action                                      \
        }                                               \
    } while (0)

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _AST_H */
