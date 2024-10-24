#ifndef _IR_H
#define _IR_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <stdint.h>

#include "vec.h"

typedef enum {
    Op_LABEL,   // l: dst
    Op_MOV_LIT, // v[dst] = a
    Op_MOV_VAR, // v[dst] = v[a]
    Op_RET,     // ret dst
    Op_JMP,     // goto dst
    Op_JMP_CND, // if v[a] then (goto dst) else (skip)
    Op_CALL,    // dst = f[a](...)
    Op_ARG_LIT, // f[dst] = a
    Op_ARG_VAR, // f[dst] = a
    Op_UNM,     // v[dst] = -v[a]
    Op_NEG,     // v[dst] = !v[a]
    Op_MUL,     // v[dst] = v[a] * v[b]
    Op_DIV,     // v[dst] = v[a] / v[b]
    Op_MOD,     // v[dst] = v[a] % v[b]
    Op_ADD,     // v[dst] = v[a] + v[b]
    Op_SUB,     // v[dst] = v[a] - v[b]
    Op_LT,      // v[dst] = v[a] < v[b]
    Op_GT,      // v[dst] = v[a] > v[b]
    Op_EQ,      // v[dst] = v[a] == v[b]
    Op_AND,     // v[dst] = v[a] & v[b]
    Op_OR,      // v[dst] = v[a] | v[b]
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
} IR;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif // _IR_H
