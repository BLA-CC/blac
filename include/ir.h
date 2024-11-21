#ifndef _IR_H
#define _IR_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "common.h"
#include "str_pool.h"
#include "sym_table.h"
#include "vec.h"

typedef uint32_t IrLoc;
typedef uint32_t IrLbl;

#define NEW_LOC 0

typedef enum {
    JmpCond_LT,
    JmpCond_GT,
    JmpCond_EQ,

    JmpCond_GE,
    JmpCond_LE,
    JmpCond_NE,
} JmpCond;

typedef enum {
    Op_LBL,        // lbl[a]:
    Op_MOV,        // v[dst] = v[a]
    Op_MOV_LIT,    // v[dst] = a

    Op_SET_GLOBAL, // g[dst] = v[a]
    Op_GET_GLOBAL, // v[dst] = g[a]

    Op_UNM,        // v[dst] = -v[a]
    Op_NEG,        // v[dst] = !v[a]

    Op_MUL,        // v[dst] = v[a] * v[b]
    Op_DIV,        // v[dst] = v[a] / v[b]
    Op_MOD,        // v[dst] = v[a] % v[b]
    Op_ADD,        // v[dst] = v[a] + v[b]
    Op_SUB,        // v[dst] = v[a] - v[b]

    Op_MUL_LIT,    // v[dst] = v[a] * b
    Op_DIV_LIT,    // v[dst] = v[a] / b
    Op_MOD_LIT,    // v[dst] = v[a] % b
    Op_ADD_LIT,    // v[dst] = v[a] + b
    Op_SUB_LIT,    // v[dst] = v[a] - b

    Op_CMP,        // flags = v[a] <=> v[b]
    Op_CMP_LIT,    // flags = v[a] <=> b

    Op_JMP,        // goto lbl[a]
    Op_JMP_IF,     // if flags == a then goto lbl[b]

    Op_CALL,       // dst = f[a](b arguments)

    Op_ARG,        // arg[dst] = v[a]
    Op_ARG_LIT,    // arg[dst] = a

    Op_RET,        // ret v[a]
    Op_RET_LIT,    // ret a
} Op;

typedef struct {
    Op op;
    uint32_t a;
    uint32_t b;
    uint32_t dst;
} Instr;

Vec_Proto(Instr);

typedef struct {
    InstrVec instrs;
    StrIdx name;
    uint32_t locals;
    uint32_t arity;
} Func;

Vec_Proto(Func);

typedef struct {
    StrIdx name;
    int32_t value;
} Global;

Vec_Proto(Global);

typedef struct {
    GlobalVec globals;
    FuncVec funcs;
} Ir;

typedef struct {
    Ir ir;
    Func *cur_func;

    uint32_t vstack_top;
    uint32_t label_gen;
    SymTable sym_table;
} IrGen;

void ir_new_func(IrGen *ir_gen, StrIdx name, uint32_t arity);

IrLoc ir_mk_loc(IrGen *ir_gen);

void ir_free_loc(IrGen *ir_gen, IrLoc v);

IrLbl ir_mk_lbl(IrGen *ir_gen);

Ir mk_ir(const Ast ast);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif // _IR_H
