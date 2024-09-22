#ifndef _STR_POOL
#define _STR_POOL

#include <stdint.h>
#include "vec.h"

typedef uint32_t StrIdx;
#define NO_STR UINT32_MAX

Vec_Proto(char);
typedef struct { charVec strs; } StrPool;

void StrPool_release(StrPool *self);

StrIdx StrPool_put(StrPool *self, const char *sym);
const char *StrPool_get(StrPool *self, StrIdx id);

#endif /* _STR_POOL */
