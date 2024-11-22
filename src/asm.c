#include "common.h"
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
    uint32_t params_in_regs = MIN(func->arity, N_ARGREGS);
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

static char *cond2jmp_insn(const JmpCond jmp_cond) {
    switch (jmp_cond) {
        case JmpCond_LT:
            return "jl";
        case JmpCond_GT:
            return "jg";
        case JmpCond_EQ:
            return "je";
        case JmpCond_GE:
            return "jge";
        case JmpCond_LE:
            return "jle";
        case JmpCond_NE:
            return "jne";
    }
    unreachable;
}

static void gen_asm_instr(const Instr instr, const Func func, StrPool strs, FILE *file) {
    switch (instr.op) {
    case Op_LBL: // .lbl[a]:
        fprintf(file, ".L%u:\n", instr.a);
        break;
    case Op_MOV: // v[dst] = v[a]
        fprintf(
            file, "\tmovl %d(%%rbp), %%r10d\n",
            var2offset(&func, instr.a)
        );
        fprintf(
            file, "\tmovl %%r10d, %d(%%rbp)\n",
            var2offset(&func, instr.dst)
        );
        break;
    case Op_MOV_LIT: // v[dst] = a
        fprintf(
            file, "\tmovl $%d, %d(%%rbp)\n",
            instr.a, var2offset(&func, instr.dst)
        );
        break;
    case Op_SET_GLOBAL: // g[dst] = v[a]
        fprintf(
            file, "\tmovl %d(%%rbp), %s(%%rip)\n",
            var2offset(&func, instr.a), StrPool_get(&strs, instr.dst)
        );
        break;
    case Op_GET_GLOBAL: // v[dst] = g[a]
        fprintf(
            file, "\tmovl %s(%%rip), %d(%%rbp)\n",
            StrPool_get(&strs, instr.dst), var2offset(&func, instr.a)
        );
        break;
    case Op_UNM: // v[dst] = -v[a]
        fprintf(file, "\tmovl %d(%%rbp), %%r10d\n", var2offset(&func, instr.a));
        fprintf(file, "\tnegl %%r10d\n");
        fprintf(file, "\tmovl %%r10d, %d(%%rbp)\n", var2offset(&func, instr.dst));
        break;
    case Op_NEG: // v[dst] = !v[a]
        fprintf(file, "\tmovl %d(%%rbp), %%r10d\n", var2offset(&func, instr.a));
        fprintf(file, "\txorl $1 %%r10d\n");
        fprintf(file, "\tmovl %%r10d,  %d(%%rbp)\n", var2offset(&func, instr.a));
        break;
    case Op_MUL: // v[dst] = v[a] * v[b]
        fprintf(file, "\tmovl %d(%%rbp), %%r10d\n", var2offset(&func, instr.a));
        fprintf(file, "\timull %d(%%rbp), %%r10d\n", var2offset(&func, instr.b));
        fprintf(file, "\tmovl %%r10d, %d(%%rbp)\n", var2offset(&func, instr.dst));
        break;
    case Op_DIV: // v[dst] = v[a] / v[b]
    case Op_MOD: // v[dst] = v[a] % v[b]
        fprintf(file, "\tmovl $0 %%edx\n");
        fprintf(file, "\tmovl %d(%%rbp) %%eax\n", var2offset(&func, instr.a));
        fprintf(file, "\tmovl %d(%%rbp), %%r10d\n", var2offset(&func, instr.b));
        fprintf(file, "\tidivl %%r10d\n");
        if (instr.op == Op_DIV) {
            fprintf(file, "\tmovl %%eax %d(%%rpb)\n", var2offset(&func, instr.dst));
        } else {
            fprintf(file, "\tmovl %%edx %d(%%rpb)\n", var2offset(&func, instr.dst));
        }
        break;
    case Op_ADD: // v[dst] = v[a] + v[b]
        fprintf(file, "\tmovl %d(%%rbp), %%r10d\n", var2offset(&func, instr.a));
        fprintf(file, "\tmovl %d(%%rbp), %%r11d\n", var2offset(&func, instr.b));
        fprintf(file, "\taddl %%r11d, %%r10d\n");
        fprintf(file, "\tmovl %%r10d, %d(%%rbp)\n", var2offset(&func, instr.dst));
        break;
    case Op_SUB: // v[dst] = v[a] - v[b]
        fprintf(file, "\tmovl %d(%%rbp), %%r10d\n", var2offset(&func, instr.a));
        fprintf(file, "\tmovl %d(%%rbp), %%r11d\n", var2offset(&func, instr.b));
        fprintf(file, "\tsubl %%r11d, %%r10d\n");
        fprintf(file, "\tmovl %%r10d, %d(%%rbp)\n", var2offset(&func, instr.dst));
        break;
    case Op_MUL_LIT: // v[dst] = v[a] * b
        fprintf(file, "\tmovl %d(%%rbp), %%r10d\n", var2offset(&func, instr.a));
        fprintf(file, "\timull $%d, %%r10d, %%r11d\n", instr.b);
        fprintf(file, "\tmovl %%r11d, %d(%%rbp)\n", var2offset(&func, instr.dst));
        break;
    case Op_DIV_LIT: // v[dst] = v[a] / b
    case Op_MOD_LIT: // v[dst] = v[a] % b
        panic("TODO");
        break;
    case Op_ADD_LIT: // v[dst] = v[a] + b
        fprintf(file, "\tmovl %d(%%rbp), %%r10d\n", var2offset(&func, instr.a));
        fprintf(file, "\taddl $%d, %%r10d\n", instr.b);
        fprintf(file, "\tmovl %%r10d, %d(%%rbp)\n", var2offset(&func, instr.dst));
        break;
    case Op_SUB_LIT: // v[dst] = v[a] - b
        fprintf(file, "\tmovl %d(%%rbp), %%r10d\n", var2offset(&func, instr.a));
        fprintf(file, "\tsubl $%d, %%r10d\n", instr.b);
        fprintf(file, "\tmovl %%r10d, %d(%%rbp)\n", var2offset(&func, instr.dst));
        break;
    case Op_CMP: // flags = v[a] <=> v[b]
        fprintf(file, "\tmovl %d(%%rbp), %%r10d\n", var2offset(&func, instr.b));
        fprintf(file, "\tmovl %d(%%rbp), %%r11d\n", var2offset(&func, instr.a));
        fprintf(file, "\tcmpl %%r11d, %%r10d\n");
        break;
    case Op_CMP_LIT: // flags = v[a] <=> b
        fprintf(file, "\tmovl %d(%%rbp), %%r10d\n", var2offset(&func, instr.a));
        fprintf(file, "\tmovl $%d, %%r11d\n", instr.b);
        fprintf(file, "\tcmpl %%r11d, %%r10d\n");
        break;
    case Op_JMP: // goto lbl[a]
        fprintf(file, "\tjmp .L%u\n", instr.a);
        break;
    case Op_JMP_IF: // if flags == a then goto lbl[b]
        fprintf(file, "\t%s .L%u\n", cond2jmp_insn(instr.a), instr.b);
        break;
    case Op_CALL: // dst = f[a](b arguments)
        {
            const char *func_label = StrPool_get(&strs, instr.a);
            fprintf(file, "\tcall %s\n", func_label);
            if (instr.dst != 0) {
                fprintf(file, "\tmovl %%eax, %d(%%rbp)\n", var2offset(&func, instr.dst));
            }
            if (instr.b > N_ARGREGS) {
                fprintf(file, "\taddq $%d, %%rsp\n",
                (instr.b - N_ARGREGS) * REG_SIZE
                );
            }
        }
        break;
    case Op_ARG: // arg[dst] = v[a]
        if (instr.dst < N_ARGREGS) {
            fprintf(
                file,"\tmovl %d(%%rbp), %%%s\n",
                var2offset(&func, instr.a), CC_REGS[instr.dst]
            );
        } else {
            fprintf(file, "push %d(%%rbp)\n", var2offset(&func, instr.a));
        }
        break;
    case Op_ARG_LIT: // arg[dst] = a
        if (instr.dst <= N_ARGREGS) {
            fprintf(
                file,"\tmovl $%d, %%%s\n",
                instr.a, CC_REGS[instr.dst]
            );
        } else {
            fprintf(file, "push $%d\n", instr.a);
        }
        break;
    case Op_RET: // ret v[a]
        fprintf(file, "\tmovl %d(%%rbp), %%eax\n", var2offset(&func, instr.a));
        fprintf(file, "\tleave\n\tret\n");
        break;
    case Op_RET_LIT: // ret a
        fprintf(file, "\tmovl $%d, %%eax\n", instr.a);
        fprintf(file, "\tleave\n\tret\n");
        break;
    }
}

static void gen_asm_func(const Func func, StrPool strs, FILE *file) {
    gen_asm_label(func.name, strs, file);
    uint32_t params_in_stack = func.arity - MIN(N_ARGREGS, func.arity);
    fprintf(file, "\tenter $(%d*%d), $0\n", REG_SIZE, func.locals - params_in_stack);

    int32_t stack_idx = -REG_SIZE;
    for (int32_t i = MIN(N_ARGREGS, func.arity) - 1; i >= 0; i--) {
        fprintf(file, "\tmovl %%%s, %d(%%rbp)\n", CC_REGS[i], stack_idx);
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
