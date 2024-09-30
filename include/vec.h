#ifndef _VEC_H
#define _VEC_H

#include <stdio.h>
#include <stdlib.h>
#include "common.h"

/**
 * Generic dynamic array.
 * Provides prototypes and implementation separately.
 */

#define Vec(T) T##Vec

#define Vec_Proto(T)                                                           \
    typedef struct T##Vec {                                                    \
        uint32_t cap;                                                          \
        uint32_t len;                                                          \
        T *elems;                                                              \
    } T##Vec;                                                                  \
    void T##Vec_push(T##Vec *vec, T data);                                     \
    T T##Vec_pop(T##Vec *vec);                                                 \
    void T##Vec_reserve(T##Vec *vec, uint32_t count);                          \
    void T##Vec_free(T##Vec *vec);

#define Vec_Impl(T)                                                            \
    void T##Vec_push(T##Vec *vec, T data) {                                    \
        if (vec->len + 1 > vec->cap) {                                         \
            uint32_t new_cap = vec->cap == 0 ? 64 : 2 * vec->cap;              \
            vec->elems = realloc(vec->elems, new_cap * sizeof(T));             \
            if (vec->elems == NULL) {                                          \
                fprintf(stderr, "UNRECOVERABLE: Out of memory\n");             \
                exit(1);                                                       \
            }                                                                  \
            vec->cap = new_cap;                                                \
        }                                                                      \
        vec->elems[vec->len++] = data;                                         \
    }                                                                          \
    T T##Vec_pop(T##Vec *vec) {                                                \
        assert(vec->len > 0);                                                  \
        return vec->elems[--vec->len];                                         \
    }                                                                          \
    void T##Vec_reserve(T##Vec *vec, uint32_t count) {                         \
        uint32_t new_len = vec->len + count;                                   \
        uint32_t new_cap = new_len;                                            \
        new_cap--;                                                             \
        new_cap |= new_cap >> 1;                                               \
        new_cap |= new_cap >> 2;                                               \
        new_cap |= new_cap >> 4;                                               \
        new_cap |= new_cap >> 8;                                               \
        new_cap |= new_cap >> 16;                                              \
        new_cap++;                                                             \
        vec->elems = realloc(vec->elems, new_cap * sizeof(T));                 \
        if (vec->elems == NULL && new_cap != 0) {                              \
            fprintf(stderr, "UNRECOVERABLE: Out of memory\n");                 \
            exit(1);                                                           \
        }                                                                      \
        vec->cap = new_cap;                                                    \
    }                                                                          \
    void T##Vec_free(T##Vec *vec) {                                            \
        vec->cap = 0;                                                          \
        vec->len = 0;                                                          \
        free(vec->elems);                                                      \
    }

#endif /* _VEC_H */
