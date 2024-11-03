#include "args.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>

#include "argparse.h"

#define TARGET_DEFAULT_STR "asm"

typedef struct argparse Argparse;

static const char *const usages[] = {
    "c-tds [options] input",
    NULL,
};

static _Noreturn void _die(Argparse *argparse, const char *fmt, ...) {
    argparse_usage(argparse);

    va_list ap;
    va_start(ap, fmt);

    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");

    va_end(ap);

    exit(EXIT_FAILURE);
}

static const char *_parse_input(Argparse *argparse, const char *argv[]) {
    const char *format = strrchr(argv[0], '.');

    if (format == NULL) {
        format = "<null>";
    }

    if (strcmp(format, ".ctds") != 0) {
        _die(argparse, "invalid file format %s", format);
    }

    return argv[0];
}

static TargetFlags _parse_single_target(
    Argparse *argparse,
    const char *target) {
    if (strcmp(target, "scan") == 0) {
        return TARGET_FLAG_SCAN;
    } else if (strcmp(target, "parse") == 0) {
        return TARGET_FLAG_PARSE;
    } else if (strcmp(target, "ir") == 0) {
        return TARGET_FLAG_IR;
    } else if (strcmp(target, "asm") == 0) {
        return TARGET_FLAG_ASM;
    }

    _die(argparse, "invalid target %s", target);
}

static TargetFlags _parse_targets(Argparse *argparse, const char *targets_str) {
    TargetFlags targets = _parse_single_target(argparse, targets_str);

    return targets;
}

static const char *_parse_opts(Argparse *argparse, const char *opt) {
    return opt;
}

Args arg_parse(int argc, const char *argv[]) {
    bool debug = false;
    const char *output = OUTPUT_DEFAULT;
    const char *opt = OPTIMIZE_DEFAULT;
    const char *targets = "asm";

    // clang-format off
    struct argparse_option options[] = {
        OPT_HELP(),
        OPT_GROUP("Compile options"),
        OPT_STRING('o', "output", &output, "output file name", NULL, 0, 0),
        OPT_STRING('t', "target", &targets, "at least one of scan|parse|ir|asm", NULL, 0, 0),
        OPT_STRING('O', "optimize", &opt, "not yet implemented", NULL, 0, 0),
        OPT_BOOLEAN('d', "debug", &debug, "allow extra debug info", NULL, 0, 0),
        OPT_END(),
    };
    // clang-format on

    Argparse argparse;
    argparse_init(&argparse, options, usages, 0);
    argc = argparse_parse(&argparse, argc, argv);

    if (argc < 1) {
        _die(&argparse, "no input file");
    }

    // clang-format off
    Args self = {
        .debug = debug,
        .output = output,
        .input = _parse_input(&argparse, argv),
        .targets = _parse_targets(&argparse, targets),
        .opt = _parse_opts(&argparse, opt),
    };
    // clang-format on

    return self;
}
