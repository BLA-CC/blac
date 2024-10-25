#include "ir.h"

void ir_new_func(IrGen *ir_gen) {
    Func new_func = {0};
    FuncVec_push(&ir_gen->ir.funcs, new_func);
    ir_gen->cur_func = &ir_gen->ir.funcs.elems[ir_gen->ir.funcs.len - 1];
}

IrVar ir_mk_var(IrGen *ir_gen) {
    ir_gen->vstack_top += 1;
    if (ir_gen->cur_func_stack_size < ir_gen->vstack_top) {
        ir_gen->cur_func_stack_size = ir_gen->vstack_top;
    }
    return ir_gen->vstack_top++;
}

void ir_free_var(IrGen *ir_gen, IrVar v) {
    assert(v == ir_gen->vstack_top - 1);
    ir_gen->vstack_top--;
}

uint32_t ir_mk_label(IrGen *ir_gen) {
    return ir_gen->label_gen++;
}


