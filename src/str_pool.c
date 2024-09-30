#include <stdlib.h>
#include <string.h>
#include "str_pool.h"

Vec_Impl(char);

void StrPool_release(StrPool *self) {
    if (self == NULL) {
        return;
    }

    charVec_free(&self->strs);
    *self = (StrPool){ 0 };
}

StrIdx StrPool_put(StrPool *self, const char *sym) {
    // already in table
    for (StrIdx i = 0; i < self->strs.len;
         i += strlen(&self->strs.elems[i]) + 1) {
        if (strcmp(sym, &self->strs.elems[i]) == 0) {
            return i;
        }
    }
    // must add to table
    const uint32_t sym_len = strlen(sym);
    charVec_reserve(&self->strs, sym_len + 1);

    const StrIdx res = self->strs.len;
    self->strs.len += sym_len + 1;

    strcpy(&self->strs.elems[res], sym);

    return res;
}

const char *StrPool_get(StrPool *self, StrIdx id) {
    if (self->strs.len <= id) {
        return NULL;
    }

    return &self->strs.elems[id];
}
