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
    [Op_MOD] = "mod", [Op_ADD] = "add", [Op_SUB] = "sub",
};

static const char *cond2str[] = {
    [JmpCond_LT] = "lt", [JmpCond_GT] = "gt", [JmpCond_EQ] = "eq",
    [JmpCond_GE] = "ge", [JmpCond_LE] = "le", [JmpCond_NE] = "ne",
};

void display_instr(Instr instr, StrPool strs, uint32_t indent, FILE *stream) {
    switch (instr.op) {
    case Op_LABEL:
        PPRINT(indent, ".L%u", instr.a);
        break;
    case Op_MOV:
        PPRINT(indent + INDENT_SZ, "t%u := t%d", instr.dst, instr.a);
        break;
    case Op_MOV_LIT:
        PPRINT(indent + INDENT_SZ, "t%u := $%d", instr.dst, instr.a);
        break;
    case Op_SET_GLOBAL:
        PPRINT(
            indent + INDENT_SZ,
            "G[%s] := t%u",
            StrPool_get(&strs, instr.dst),
            instr.a);
        break;
    case Op_GET_GLOBAL:
        PPRINT(
            indent + INDENT_SZ,
            "t%u := G[%s]",
            instr.dst,
            StrPool_get(&strs, instr.a));
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
        PPRINT(
            indent + INDENT_SZ,
            "t%u := %s t%u t%u",
            instr.dst,
            op_2_str[instr.op],
            instr.a,
            instr.b);
        break;
    case Op_MUL_LIT:
    case Op_DIV_LIT:
    case Op_MOD_LIT:
    case Op_ADD_LIT:
    case Op_SUB_LIT:
        PPRINT(
            indent + INDENT_SZ,
            "t%u := %s t%u $%d",
            instr.dst,
            op_2_str[instr.op - (Op_MUL_LIT - Op_MUL)],
            instr.a,
            instr.b);
        break;
    case Op_CMP:
        PPRINT(indent + INDENT_SZ, "cmp t%u t%u", instr.a, instr.b);
        break;
    case Op_CMP_LIT:
        PPRINT(indent + INDENT_SZ, "cmp t%u $%d", instr.a, instr.b);
        break;
    case Op_JMP:
        PPRINT(indent + INDENT_SZ, "jmp .L%u", instr.a);
        break;
    case Op_JMP_IF:
        PPRINT(
            indent + INDENT_SZ,
            "if flags == %s then jmp .L%u",
            cond2str[instr.a],
            instr.b);
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
    case Op_ARG_LIT:
        PPRINT(indent + INDENT_SZ, "arg[%u] := $%d", instr.dst, instr.a);
        break;
    case Op_RET:
        PPRINT(indent + INDENT_SZ, "ret t%u", instr.a);
        break;
    case Op_RET_LIT:
        PPRINT(indent + INDENT_SZ, "ret $%u", instr.a);
        break;
    }
}

void display_ir(const Ir ir, StrPool strs, uint32_t indent, FILE *stream) {
    GlobalVec globals = ir.globals;

    PPRINT(indent, "G := {");
    for (uint32_t i = 0; i < globals.len; ++i) {
        Global glbl = globals.elems[i];
        PPRINT(indent + INDENT_SZ, "%s: $%d", StrPool_get(&strs, glbl.name), glbl.value);
    }
    PPRINT(indent, "}");

    PPRINT(indent, "");

    FuncVec funcs = ir.funcs;

    for (uint32_t i = 0; i < funcs.len; i++) {
        Func func = funcs.elems[i];
        PPRINT(
            indent,
            "%s (%u params, %u locals)",
            StrPool_get(&strs, func.name),
            func.arity,
            func.locals);

        InstrVec instrs = func.instrs;
        for (uint32_t j = 0; j < instrs.len; j++) {
            display_instr(instrs.elems[j], strs, indent + INDENT_SZ, stream);
        }

        fprintf(stream, "\n");
    }
}
