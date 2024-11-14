#ifndef _ASM_H
#define _ASM_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "ir.h"

void gen_asm(const Ir ir, StrPool strs, FILE *file);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif // _ASM_H