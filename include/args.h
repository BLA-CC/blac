#ifndef _ARGS_H
#define _ARGS_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <stdbool.h>
#include <stdint.h>

#define TARGET_FLAG_SCAN  0x1
#define TARGET_FLAG_PARSE 0x2
#define TARGET_FLAG_IR    0x4
#define TARGET_FLAG_ASM   0x8
#define TARGET_ALL        0xF

#define OUTPUT_DEFAULT   "a.out"
#define TARGET_DEFAULT   TARGET_FLAG_ASM
#define OPTIMIZE_DEFAULT "none"

typedef uint32_t TargetFlags;

typedef struct {
    const char *input;
    const char *output;
    const char *opt;
    TargetFlags targets;
    bool debug;
} Args;

Args arg_parse(int argc, const char *argv[]);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _ARGS_H */
