#include "ir.h"
#include "display.h"
#include "str_pool.h"
#include <stdint.h>
#include <stdio.h>
#include <sys/ucontext.h>

#define PPRINT(indent, fmt, ...)                                               \
    fprintf(stream, "%*s" fmt "\n", indent, "" __VA_OPT__(, ) __VA_ARGS__)

static const char *op_2_str[] = {
    [Op_UNM] = "unm", [Op_NEG] = "neg", [Op_MUL] = "mul", [Op_DIV] = "div",
    [Op_MOD] = "mod", [Op_ADD] = "add", [Op_SUB] = "sub", [Op_LT] = "lt",
    [Op_GT] = "gt",   [Op_EQ] = "eq",   [Op_AND] = "and", [Op_OR] = "or",
};

void display_instr(Instr instr, StrPool strs, uint32_t indent, FILE *stream) {
    switch (instr.op) {
    case Op_LABEL:
        PPRINT(indent, ".L%u", instr.a);
        break;
    case Op_MOV_LIT:
        PPRINT(indent + INDENT_SZ, "t%u := $%d", instr.dst, instr.a);
        break;
    case Op_MOV_VAR:
        PPRINT(indent + INDENT_SZ, "t%u := t%d", instr.dst, instr.a);
        break;
    case Op_RET:
        PPRINT(indent + INDENT_SZ, "ret t%u", instr.a);
        break;
    case Op_JMP:
        PPRINT(indent + INDENT_SZ, "jmp .L%u", instr.a);
        break;
    case Op_JMP_IF_F:
        PPRINT(indent + INDENT_SZ, "if !t%u then jmp .L%u", instr.a, instr.b);
        break;
    case Op_JMP_IF_T:
        PPRINT(indent + INDENT_SZ, "if t%u then jmp .L%u", instr.a, instr.b);
        break;
    case Op_CALL:
        PPRINT(
            indent + INDENT_SZ,
            "t%u := call %s %u",
            instr.dst,
            StrPool_get(&strs, instr.a),
            instr.b);
        break;
    case Op_ARG:
        PPRINT(indent + INDENT_SZ, "arg[%u] := t%u", instr.dst, instr.a);
        break;
    case Op_UNM:
    case Op_NEG:
        PPRINT(
            indent + INDENT_SZ,
            "t%u := %s t%u",
            instr.dst,
            op_2_str[instr.op],
            instr.a);
        break;
    case Op_MUL:
    case Op_DIV:
    case Op_MOD:
    case Op_ADD:
    case Op_SUB:
    case Op_LT:
    case Op_GT:
    case Op_EQ:
    case Op_AND:
    case Op_OR:
        PPRINT(
            indent + INDENT_SZ,
            "t%u := %s t%u t%u",
            instr.dst,
            op_2_str[instr.op],
            instr.a,
            instr.b);
        break;
    case Op_SET_GLOBAL:
        PPRINT(
            indent + INDENT_SZ,
            "g_%s := t%u",
            StrPool_get(&strs, instr.dst),
            instr.a);
        break;
    case Op_GET_GLOBAL:
        PPRINT(
            indent + INDENT_SZ,
            "t%u := g_%s",
            instr.dst,
            StrPool_get(&strs, instr.a));
        break;
    }
}

void display_ir(const Ir ir, StrPool strs, uint32_t indent, FILE *stream) {
    GlobalVec globals = ir.globals;

    for (uint32_t i = 0; i < globals.len; ++i) {
        Global glbl = globals.elems[i];
        PPRINT(indent, "g_%s: $%d", StrPool_get(&strs, glbl.name), glbl.value);
    }

    PPRINT(indent, "");

    FuncVec funcs = ir.funcs;

    for (uint32_t i = 0; i < funcs.len; i++) {
        Func func = funcs.elems[i];
        PPRINT(
            indent,
            "%s (%u locals)",
            StrPool_get(&strs, func.name),
            func.locals);

        InstrVec instrs = func.instrs;
        for (uint32_t j = 0; j < instrs.len; j++) {
            display_instr(instrs.elems[j], strs, indent + INDENT_SZ, stream);
        }

        fprintf(stream, "\n");
    }
}
