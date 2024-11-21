#include "ir.h"
#include "str_pool.h"
#include <stdint.h>
#include <stdio.h>
#include "asm.h"
#define MIN(a, b) ((a) < (b) ? (a) : (b))

const char *CC_REGS[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
uint32_t CC_REGS_SIZE = 6;
uint32_t REG_SIZE = 8;


static void gen_asm_label(const StrIdx label, StrPool strs, FILE *file) {
    const char *label_str = StrPool_get(&strs, label);
    fprintf(file, "%s:\n", label_str);
}

static void gen_asm_func(const Func func, StrPool strs, FILE *file) {
    gen_asm_label(func.name, strs, file);
    fprintf(file, "\tenter $(%d*%d), $0\n", REG_SIZE, func.locals);

    int32_t stack_idx = -REG_SIZE;
    for (int32_t i = MIN(CC_REGS_SIZE, func.arity) - 1; i >= 0; i--) {
        fprintf(file, "\tmov %s %d(%%rbp)\n", CC_REGS[i], stack_idx);
        stack_idx -= REG_SIZE;
    }

    return;
}

static void gen_asm_globls(const GlobalVec globls, StrPool strs, FILE *file) {
    if (globls.len == 0) {
        return;
    }

    fprintf(file, "\t.data\n");
    for (uint32_t i = 0; i < globls.len; i++) {
        Global globl = globls.elems[i];
        const char *globl_name = StrPool_get(&strs, globl.name);

        fprintf(file,
            "\t.globl %s\n"
            "\t.align 4\n"
            "\t.type %s, @object"
            "\n\t.size %s, 4\n",
            globl_name, globl_name, globl_name);

        fprintf(file,
            "%s:\n"
            "\t.long %d\n",
            globl_name, globl.value);
    }
}

static void gen_asm_funcs(const FuncVec funcs, StrPool strs, FILE *file) {
    for (uint32_t i = 0; i < funcs.len; i++) {
        gen_asm_func(funcs.elems[i], strs, file);
    }
}

void gen_asm(const Ir ir, StrPool strs, FILE *file) {
    gen_asm_globls(ir.globals, strs, file);
    gen_asm_funcs(ir.funcs, strs, file);
}
