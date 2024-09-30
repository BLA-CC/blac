#ifndef _COMMON_H
#define _COMMON_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <assert.h>
#include <stdint.h>
#include <stdbool.h>

typedef enum {
    Type_VOID,
    Type_INT,
    Type_BOOL,
} Type;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _COMMON_H */
