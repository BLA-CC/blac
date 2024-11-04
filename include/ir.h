#ifndef _IR_H
#define _IR_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "common.h"
#include "str_pool.h"
#include "sym_table.h"
#include "vec.h"

typedef uint32_t IrVar;

Vec_Proto(IrVar);

typedef enum {
    Op_LABEL,      // .L{func}{a}:
    Op_MOV_LIT,    // v[dst] = a
    Op_MOV_VAR,    // v[dst] = v[a]
    Op_RET,        // ret a
    Op_JMP,        // goto .L{func}{a}
    Op_JMP_IF_F,   // if !v[a] then goto b
    Op_JMP_IF_T,   // if v[a] then goto b
    Op_CALL,       // dst = f[a](n arguments)
    Op_ARG,        // f[dst] = a
    Op_UNM,        // v[dst] = -v[a]
    Op_NEG,        // v[dst] = !v[a]
    Op_MUL,        // v[dst] = v[a] * v[b]
    Op_DIV,        // v[dst] = v[a] / v[b]
    Op_MOD,        // v[dst] = v[a] % v[b]
    Op_ADD,        // v[dst] = v[a] + v[b]
    Op_SUB,        // v[dst] = v[a] - v[b]
    Op_LT,         // v[dst] = v[a] < v[b]
    Op_GT,         // v[dst] = v[a] > v[b]
    Op_EQ,         // v[dst] = v[a] == v[b]
    Op_AND,        // v[dst] = v[a] & v[b]
    Op_OR,         // v[dst] = v[a] | v[b]
    Op_SET_GLOBAL, // g[dst] = v[a]
    Op_GET_GLOBAL, // v[dst] = g[a]
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

void ir_new_func(IrGen *ir_gen);

IrVar ir_mk_var(IrGen *ir_gen);

void ir_free_var(IrGen *ir_gen, IrVar v);

uint32_t ir_mk_label(IrGen *ir_gen);

Ir mk_ir(const Ast ast);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif // _IR_H
