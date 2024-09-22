#ifndef _AST_H
#define _AST_H

#include <stdint.h>
#include "common.h"
#include "str_pool.h"
#include "vec.h"

typedef struct {
    uint32_t line;
    uint32_t col;
} Location;

typedef enum {
    BinOp_MUL, BinOp_DIV, BinOp_MOD,
    BinOp_ADD, BinOp_SUB,
    BinOp_LT, BinOp_GT,
    BinOp_EQ,
    BinOp_AND,
    BinOp_OR,
} BinOp;

typedef enum {
    UnOp_UNM,
    UnOp_NEG,
} UnOp;

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

#define FOR_AST_NODES(DO)                                               \
    /* toplevel */                                                      \
    DO(PROG, struct { NodeIdx vars; NodeIdx meths; })                   \
    DO(VAR_DECL, struct { Type type; StrIdx ident; NodeIdx expr; })     \
    DO(METH_DECL, struct { StrIdx ident; NodeIdx proto; NodeIdx body; })\
    DO(METH_PROTO, struct { NodeIdx params; Type ret_type; })           \
    DO(PARAM, struct { Type type; StrIdx ident; })                      \
    /* statements */                                                    \
    DO(ASGN, struct { StrIdx var_ident; NodeIdx expr; })                \
    DO(IF, struct { NodeIdx cond; NodeIdx then_b; NodeIdx else_b; })    \
    DO(WHILE, struct { NodeIdx cond; NodeIdx body; })                   \
    DO(RET, struct { NodeIdx ret_val; })                                \
    DO(BLOCK, struct { NodeIdx vars; NodeIdx stmts; })                  \
    DO(NOP, struct {})                                                  \
    /* expressions */                                                   \
    DO(METH_CALL, struct { StrIdx meth_ident; NodeIdx args; })          \
    DO(VAR, struct { StrIdx ident; })                                   \
    DO(INT_LIT, struct { int64_t lit_val; })                            \
    DO(BOOL_LIT, struct { bool lit_val; })                              \
    DO(UNOP, struct { NodeIdx arg; UnOp op; })                          \
    DO(BINOP, struct { NodeIdx lhs; NodeIdx rhs; BinOp op; })           \
    /* lists */                                                         \
    DO(LIST, struct { NodeIdx begin; NodeIdx end; })                    \


#define MK_KIND(name, type) AstNodeKind_ ## name,
typedef enum {
    FOR_AST_NODES(MK_KIND)
} AstNodeKind;
#undef MK_KINDS

#define MK_DATA(name, type) typedef type AstNodeData_ ## name;
FOR_AST_NODES(MK_DATA)
#undef MK_DATA

#define MK_UNION_FIELD(name, type) AstNodeData_ ## name name;
typedef union {
    FOR_AST_NODES(MK_UNION_FIELD)
} AstNodeData;
#undef MK_UNION_FIELD

typedef struct {
    Location loc;
    AstNodeKind kind;
    AstNodeData data;
} AstNode;

#define AstNode_mk(l, k, ds...)     \
    (AstNode){                      \
        .loc = l,                   \
        .kind = AstNodeKind_ ## k,  \
        .data = { .k = { ds } }     \
    }

Vec_Proto(AstNode);

NodeIdx AstBuilder_mk_list(AstNodeVec *ast_nodes, AstNodeVec *list_nodes, uint32_t begin);

#endif /* _AST_H */
