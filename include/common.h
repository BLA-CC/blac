#ifndef _COMMON_H
#define _COMMON_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

typedef enum {
    Type_NONE,
    Type_VOID,
    Type_INT,
    Type_BOOL,
} Type;

#define panic(msg, ...)                                                        \
    do {                                                                       \
        fprintf(stderr, msg "\n" __VA_OPT__(, ) __VA_ARGS__);                  \
        exit(EXIT_FAILURE);                                                    \
    } while (0)

#define unreachable assert(false && "reached unreachable code")

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _COMMON_H */
