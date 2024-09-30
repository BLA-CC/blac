#ifndef _ARGS_H
#define _ARGS_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <stdbool.h>

typedef struct {
    char *input;
    char *output;
    char *target;
    char *opt;
    bool debug;
} Args;

Args arg_parse(int argc, char *argv[]);

void arg_release(Args *self);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _ARGS_H */
