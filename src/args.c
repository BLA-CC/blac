#include "args.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>

#include "argparse.h"

typedef struct argparse Argparse;

#define SET_DEFAULT_OPTION(opt, val)                                           \
    if ((opt) == NULL) {                                                       \
        opt = val;                                                             \
    }

static char *OUTPUT_DEFAULT = "a.out";
static char *TARGET_DEFAULT = "asm";
static char *OPTIMIZE_DEFAULT = "none";

static const char *const usages[] = {
    "c-tds [options] input",
    NULL,
};

/*
static char *u_strdup(const char *input) {
    size_t n = strlen(input);
    char *output = calloc(n + 1, sizeof(*output));
    memcpy(output, input, n);
    return output;
}
*/

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

static const char *
_parse_input(Argparse *argparse, int argc, const char *argv[]) {
    if (argc < 1) {
        _die(argparse, "no input file");
    }

    const char *format = strrchr(argv[0], '.');

    if (format == NULL) {
        format = "<null>";
    }

    if (strcmp(format, ".ctds") != 0) {
        _die(argparse, "invalid file format %s", format);
    }

    return argv[0];
}

static Target _parse_target(Argparse *argparse, const char *target) {
    if (strcmp(target, "scan") == 0) {
        return Target_SCAN;
    } else if (strcmp(target, "parse") == 0) {
        return Target_PARSE;
    } else if (strcmp(target, "ir") == 0) {
        return Target_IR;
    } else if (strcmp(target, "asm") == 0) {
        return Target_ASM;
    }

    _die(argparse, "invalid target %s", target);
}

Args arg_parse(int argc, char *argv[]) {
    Args self = { 0 };

    char *target = NULL;

    // clang-format off
    struct argparse_option options[] = {
        OPT_HELP(),
        OPT_GROUP("Compile options"),
        OPT_STRING('o', "output", &self.output, "output file", NULL, 0, 0),
        OPT_STRING('t', "target", &target, "scan|parse|ir|asm", NULL, 0, 0),
        OPT_STRING('O', "optimize", &self.opt, "none|all", NULL, 0, 0),
        OPT_BOOLEAN('d', "debug", &self.debug, "allow extra debug info", NULL, 0, 0),
        OPT_END(),
    };
    // clang-format on

    Argparse argparse;
    argparse_init(&argparse, options, usages, 0);
    argc = argparse_parse(&argparse, argc, (const char **)argv);

    self.input = _parse_input(&argparse, argc, (const char **)argv);

    SET_DEFAULT_OPTION(target, TARGET_DEFAULT);
    self.target = _parse_target(&argparse, target);

    SET_DEFAULT_OPTION(self.output, OUTPUT_DEFAULT);
    SET_DEFAULT_OPTION(self.opt, OPTIMIZE_DEFAULT);

    return self;
}

void arg_release(Args *self) {
    (void)self;
}
