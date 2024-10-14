#ifndef _ARGS_H
#define _ARGS_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <stdbool.h>

typedef enum {
    Target_SCAN,
    Target_PARSE,
    Target_IR,
    Target_ASM,
} Target;

typedef struct {
    const char *input;
    const char *output;
    const char *opt;
    Target target;
    bool debug;
} Args;

Args arg_parse(int argc, char *argv[]);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _ARGS_H */
