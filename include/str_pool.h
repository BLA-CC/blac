#ifndef _STR_POOL
#define _STR_POOL

#include <stdint.h>
#include "vec.h"

typedef uint32_t StrIdx;
#define NO_STR UINT32_MAX

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

Vec_Proto(char);

typedef struct {
    Vec(char) strs;
} StrPool;

void StrPool_release(StrPool *self);

StrIdx StrPool_put(StrPool *self, const char *sym);
const char *StrPool_get(StrPool *self, StrIdx id);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _STR_POOL */
