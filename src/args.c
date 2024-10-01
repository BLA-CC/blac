#include "args.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

#include "argparse.h"

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

static char *u_strdup(const char *input) {
    size_t n = strlen(input);
    char *output = calloc(n + 1, sizeof(*output));
    memcpy(output, input, n);
    return output;
}

Args arg_parse(int argc, char *argv[]) {
    Args self = { 0 };

    // clang-format off
    struct argparse_option options[] = {
        OPT_HELP(),
        OPT_GROUP("Compile options"),
        OPT_STRING('o', "output", &self.output, "output file", NULL, 0, 0),
        OPT_STRING('t', "target", &self.target, "scan|parse|ir|asm", NULL, 0, 0),
        OPT_STRING('O', "optimize", &self.opt, "none|all", NULL, 0, 0),
        OPT_BOOLEAN('d', "debug", &self.debug, "allow extra debug info", NULL, 0, 0),
        OPT_END(),
    };
    // clang-format on

    struct argparse argparse;
    argparse_init(&argparse, options, usages, 0);
    argc = argparse_parse(&argparse, argc, (const char **)argv);

    if (argc < 1) {
        argparse_usage(&argparse);
        fprintf(stderr, "error: no input files\n");
        exit(1);
    }
    self.input = u_strdup(argv[0]);

    SET_DEFAULT_OPTION(self.output, OUTPUT_DEFAULT);
    SET_DEFAULT_OPTION(self.target, TARGET_DEFAULT);
    SET_DEFAULT_OPTION(self.opt, OPTIMIZE_DEFAULT);

    return self;
}

void arg_release(Args *self) {
    free(self->input);
}
