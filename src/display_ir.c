#include "ir.h"
#include "display.h"
#include "str_pool.h"
#include <stdint.h>
#include <stdio.h>
#include <sys/ucontext.h>

void display_instr(const Instr inst, StrPool strs, FILE *stream) {
    char *op;
    switch (inst.op) {
    case Op_LABEL:
        op = "LABEL";
        break;
    case Op_MOV_LIT:
        op = "MOV_LIT";
        break;
    case Op_MOV_VAR:
        op = "MOV_VAR";
        break;
    case Op_RET:
        op = "RET";
        break;
    case Op_JMP:
        op = "JMP";
        break;
    case Op_JMP_CND:
        op = "JMP_CND";
        break;
    case Op_CALL:
        op = "CALL";
        break;
    case Op_ARG_LIT:
        op = "ARG_LIT";
        break;
    case Op_ARG_VAR:
        op = "ARG_VAR";
        break;
    case Op_UNM:
        op = "UNM";
        break;
    case Op_NEG:
        op = "NEG";
        break;
    case Op_MUL:
        op = "MUL";
        break;
    case Op_DIV:
        op = "DIV";
        break;
    case Op_MOD:
        op = "MOD";
        break;
    case Op_ADD:
        op = "ADD";
        break;
    case Op_SUB:
        op = "SUB";
        break;
    case Op_LT:
        op = "LT";
        break;
    case Op_GT:
        op = "GT";
        break;
    case Op_EQ:
        op = "EQ";
        break;
    case Op_AND:
        op = "AND";
        break;
    case Op_OR:
        op = "OR";
        break;
    }
    fprintf(stream,"%s\tdst[%d]\ta[%d]\tb[%d]\n", op, inst.dst, inst.a,
    inst.b);
}

void display_ir(const Ir ir, StrPool strs, FILE *stream) {
    // TODO: make it less ugly
    FuncVec funcs = ir.funcs;
    Func curr;
    for (uint32_t i = 0; i < funcs.len; i++) {
        curr = funcs.elems[i];
        fprintf(stream, "\n%s:\n", StrPool_get(&strs, curr.name));
        
        for (uint32_t j = 0; j < curr.instrs.len; j++) {
            display_instr(curr.instrs.elems[j], strs, stream);
        }
    }
}
