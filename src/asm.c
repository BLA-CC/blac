#include "ir.h"
#include "str_pool.h"
#include <stdint.h>
#include <stdio.h>
#include "asm.h"

static void gen_asm_func(const Func func, StrPool strs, FILE *file) {
    const char *func_name = StrPool_get(&strs, func.name);
    fprintf(file, "%s:\n// TODO\n", func_name);
    return;
}

static void gen_asm_globls(const GlobalVec globls, StrPool strs, FILE *file) {
    fprintf(file, "\t.text\n");
    for (uint32_t i = 0; i < globls.len; i++) {
        Global globl = globls.elems[i];
        const char *globl_name = StrPool_get(&strs, globl.name);

        fprintf(file,
            "\t.globl %s\n"
            "\t.data\n"
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
