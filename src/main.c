#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "args.h"
#include "tyck.h"
#include "str_pool.h"
#include "parser.h"
#include "display.h"
#include "ir.h"
#include "asm.h"

int main(int argc, char *argv[]) {
    Args args = arg_parse(argc, (const char **)argv);

    StrPool strs = { 0 };
    Parser parser = { 0 };

    char out_filename[256];

    int exit_status = EXIT_SUCCESS;

    yyscan_t scanner;
    if (yylex_init_extra(&strs, &scanner)) {
        exit_status = EXIT_FAILURE;
        goto scan_init_failure;
    }

    FILE *input_file;
    if ((input_file = fopen(args.input, "r")) == NULL) {
        fprintf(stderr, "error: nonexistent input file (%s)\n", args.input);
        exit_status = EXIT_FAILURE;
        goto input_file_failure;
    }

    yyset_in(input_file, scanner);

    if (args.targets & TARGET_FLAG_SCAN) {
        sprintf(out_filename, "%s.lex", args.input);

        FILE *output_file;
        if ((output_file = fopen(out_filename, "w")) == NULL) {
            fprintf(stderr, "error: could not create output file\n");
            exit(EXIT_FAILURE);
        }

        display_toks(scanner, strs, output_file);

        fclose(output_file);

        goto stage_scan_cleanup;
    }

    if (yyparse(&parser, scanner)) {
        exit_status = EXIT_FAILURE;
        goto parse_init_failure;
    }

    Ast ast = Parser_mk_ast(&parser);

    if (args.targets & TARGET_FLAG_PARSE) {
        sprintf(out_filename, "%s.sint", args.input);

        FILE *output_file;
        if ((output_file = fopen(out_filename, "w")) == NULL) {
            fprintf(stderr, "error: could not create output file\n");
            exit(EXIT_FAILURE);
        }
        display_ast(ast, strs, 0, output_file);

        fclose(output_file);

        goto stage_parse_cleanup;
    }

    if (!tyck(ast, strs)) {
        exit_status = EXIT_FAILURE;
        goto stage_sempass_error;
    }

    Ir ir = mk_ir(ast);

    if (args.targets & TARGET_FLAG_IR) {
        sprintf(out_filename, "%s.ir", args.input);

        FILE *output_file;
        if ((output_file = fopen(out_filename, "w")) == NULL) {
            fprintf(stderr, "error: could not create output file\n");
            exit(EXIT_FAILURE);
        }
        display_ir(ir, strs, 0, output_file);

        fclose(output_file);

        goto stage_ir_cleanup;
    }

    if (args.targets & TARGET_FLAG_ASM) {
        sprintf(out_filename, "%s.asm", args.input);

        FILE *output_file;
        if ((output_file = fopen(out_filename, "w")) == NULL) {
            fprintf(stderr, "error: could not create output file\n");
            exit(EXIT_FAILURE);
        }
        gen_asm(ir, strs, output_file);
        fclose(output_file);
    }

stage_ir_cleanup:
    for (Func *func = &ir.funcs.elems[0]; func < &ir.funcs.elems[ir.funcs.len];
         func++) {
        InstrVec_free(&func->instrs);
    }
    FuncVec_free(&ir.funcs);

stage_sempass_error:
stage_parse_cleanup:
    StrPool_release(&strs);
    Ast_release(&ast);

parse_init_failure:
stage_scan_cleanup:
    fclose(input_file);

input_file_failure:
    yylex_destroy(scanner);

scan_init_failure:

    return exit_status;
}
