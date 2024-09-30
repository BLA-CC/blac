#ifndef _STR_POOL
#define _STR_POOL

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <stdint.h>
#include "vec.h"

typedef uint32_t StrIdx;
#define NO_STR UINT32_MAX

Vec_Proto(char);

/**
 * The `StrPool` provides a way to intern strings. This in turn makes testing
 * for (interned) string equality much faster, besides from decreasing the size
 * of "pointers" from 64 to 32 bits.
 */
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
