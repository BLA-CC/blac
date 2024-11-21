#include "ir.h"
#include "str_pool.h"
#include <stdint.h>
#include <stdio.h>
#include "asm.h"
#define MIN(a, b) ((a) < (b) ? (a) : (b))

const char *CC_REGS[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
uint32_t N_ARGREGS = 6;
uint32_t REG_SIZE = 4;

static int32_t var2offset(const Func *func, uint32_t var) {
    uint32_t params_in_regs = func->arity < N_ARGREGS ? func->arity : N_ARGREGS;
    uint32_t params_in_stack = func->arity - params_in_regs;
    // param
    if (var <= params_in_regs) return -var * REG_SIZE;
    if (var <= func->arity) return (var - params_in_regs) * REG_SIZE;
    // local
    return -(var - params_in_stack) * REG_SIZE;
}

static void gen_asm_label(const StrIdx label, StrPool strs, FILE *file) {
    const char *label_str = StrPool_get(&strs, label);
    fprintf(file, "%s:\n", label_str);
}

static void gen_asm_instr(const Instr instr, const Func func, StrPool strs, FILE *file) {
    switch (instr.op) {
    case Op_LBL: // .lbl[a]:
        fprintf(file, ".L%d:\n", instr.a);
        break;
    case Op_MOV: // v[dst] = v[a]
        fprintf(
            file, "\tmovl %d(%%ebp), %d(%%ebp)\n",
            var2offset(&func, instr.a), var2offset(&func, instr.dst)
        );
        break;
    case Op_MOV_LIT: // v[dst] = a
        fprintf(
            file, "\tmovl $%d, %d(%%ebp)\n",
            instr.a, var2offset(&func, instr.dst)
        );
        break;
    case Op_SET_GLOBAL: // g[dst] = v[a]
        fprintf(
            file, "\tmovl %d(%%ebp), %s(%%rip)\n",
            var2offset(&func, instr.a), StrPool_get(&strs, instr.dst)
        );
        break;
    case Op_GET_GLOBAL: // v[dst] = g[a]
        fprintf(
            file, "\tmovl %s(%%rip), %d(%%ebp)\n",
            StrPool_get(&strs, instr.dst), var2offset(&func, instr.a)
        );
        break;
    case Op_UNM: // v[dst] = -v[a]
    case Op_NEG: // v[dst] = !v[a]
        fprintf(file, "\tmovl %d(%%ebp), %%r10\n", var2offset(&func, instr.a));
        fprintf(file, "\tnegl %%r10\n");
        fprintf(file, "\tmovl %%r10, %d(%%ebp)\n", var2offset(&func, instr.dst));
        break;
    case Op_MUL: // v[dst] = v[a] * v[b]
        fprintf(file, "\tmovl %d(%%ebp), %%r10\n", var2offset(&func, instr.a));
        fprintf(file, "\timull %d(%%ebp), %%r10\n", var2offset(&func, instr.b));
        fprintf(file, "\tmovl %%r10, %d(%%ebp)\n", var2offset(&func, instr.dst));
        break;
    case Op_DIV: // v[dst] = v[a] / v[b]
        // TODO
        break;
    case Op_MOD: // v[dst] = v[a] % v[b]
        // TODO
        break;
    case Op_ADD: // v[dst] = v[a] + v[b]
        fprintf(file, "\tmovl %d(%%ebp), %%r10\n", var2offset(&func, instr.a));
        fprintf(file, "\tmovl %d(%%ebp), %%r11\n", var2offset(&func, instr.b));
        fprintf(file, "\taddl %%r11, %%r10\n");
        fprintf(file, "\tmovl %%r10, %d(%%ebp)\n", var2offset(&func, instr.dst));
      break;
    case Op_SUB: // v[dst] = v[a] - v[b]
        fprintf(file, "\tmovl %d(%%ebp), %%r10\n", var2offset(&func, instr.a));
        fprintf(file, "\tmovl %d(%%ebp), %%r11\n", var2offset(&func, instr.b));
        fprintf(file, "\tsubl %%r11, %%r10\n");
        fprintf(file, "\tmovl %%r10, %d(%%ebp)\n", var2offset(&func, instr.dst));
        break;
    case Op_MUL_LIT: // v[dst] = v[a] * b
        fprintf(file, "\tmovl %d(%%ebp), %%r10\n", var2offset(&func, instr.a));
        fprintf(file, "\timull $%d, %%r10, %%r11\n", instr.b);
        fprintf(file, "\tmovl %%r11, %d(%%ebp)\n", var2offset(&func, instr.dst));
        break;
    case Op_DIV_LIT: // v[dst] = v[a] / b
        // TODO
        break;
    case Op_MOD_LIT: // v[dst] = v[a] % b
        // TODO
        break;
    case Op_ADD_LIT: // v[dst] = v[a] + b
        fprintf(file, "\tmovl %d(%%ebp), %%r10\n", var2offset(&func, instr.a));
        fprintf(file, "\taddl $%d, %%r10\n", instr.b);
        fprintf(file, "\tmovl %%r10, %d(%%ebp)\n", var2offset(&func, instr.dst));
        break;
    case Op_SUB_LIT: // v[dst] = v[a] - b
        fprintf(file, "\tmovl %d(%%ebp), %%r10\n", var2offset(&func, instr.a));
        fprintf(file, "\tsubl $%d, %%r10\n", instr.b);
        fprintf(file, "\tmovl %%r10, %d(%%ebp)\n", var2offset(&func, instr.dst));
        break;
    case Op_CMP: // flags = v[a] <=> v[b]
        fprintf(file, "\tmovl %d(%%ebp), %%r10\n", var2offset(&func, instr.b));
        fprintf(file, "\tmovl %d(%%ebp), %%r11\n", var2offset(&func, instr.a));
        fprintf(file, "\tcmp %%r10, %%r11\n");
        break;
    case Op_CMP_LIT: // flags = v[a] <=> b
        fprintf(file, "\tmovl %d(%%ebp), %%r10\n", var2offset(&func, instr.a));
        fprintf(file, "\tmovl %d, %%r11\n", instr.b);
        fprintf(file, "\tcmp %%r10, %%r11\n");
        break;
    case Op_JMP: // goto lbl[a]
        fprintf(file, "\tjmp .L%d", instr.a);
        break;
    case Op_JMP_IF: // if flags == a then goto lbl[b]
        char *jmp_insn;
        switch (instr.a) {
            case JmpCond_LT:
                jmp_insn = "jl";
                break;
            case JmpCond_GT:
                jmp_insn = "jg";
                break;
            case JmpCond_EQ:
                jmp_insn = "je";
                break;
            case JmpCond_GE:
                jmp_insn = "jge";
                break;
            case JmpCond_LE:
                jmp_insn = "jle";
                break;
            case JmpCond_NE:
                jmp_insn = "jne";
                break;
        }
        fprintf(file, "\t%s .L%d\n", jmp_insn, instr.b);
        break;
    case Op_CALL: // dst = f[a](b arguments)
        // TODO
        break;
    case Op_ARG: // arg[dst] = v[a]
        // TODO
        break;
    case Op_ARG_LIT: // arg[dst] = a
        // TODO
        break;
    case Op_RET: // ret v[a]
        fprintf(file, "\tmovl %d(%%ebp), %%eax\n", var2offset(&func, instr.a));
        fprintf(file, "\tleave\n\tret\n");
        break;
    case Op_RET_LIT: // ret a
        fprintf(file, "\tmovl %d, %%eax\n", instr.a);
        fprintf(file, "\tleave\n\tret\n");
        break;
    }
}

static void gen_asm_func(const Func func, StrPool strs, FILE *file) {
    gen_asm_label(func.name, strs, file);
    uint32_t params_in_regs = func.arity < N_ARGREGS ? func.arity : N_ARGREGS;
    fprintf(file, "\tenter $(%d*%d), $0\n", REG_SIZE, func.locals - params_in_regs);

    int32_t stack_idx = -REG_SIZE;
    for (int32_t i = MIN(N_ARGREGS, func.arity) - 1; i >= 0; i--) {
        fprintf(file, "\tmov %s %d(%%ebp)\n", CC_REGS[i], stack_idx);
        stack_idx -= REG_SIZE;
    }

    for (uint32_t i = 0; i < func.instrs.len; i++) {
        gen_asm_instr(func.instrs.elems[i], func, strs, file);
    }

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
    fprintf(file, "\t.text\n\t.globl main\n");
    for (uint32_t i = 0; i < funcs.len; i++) {
        gen_asm_func(funcs.elems[i], strs, file);
    }
}

void gen_asm(const Ir ir, StrPool strs, FILE *file) {
    gen_asm_globls(ir.globals, strs, file);
    gen_asm_funcs(ir.funcs, strs, file);
}
