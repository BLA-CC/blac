#include <stdio.h>
#include <string.h>

#include "args.h"
#include "str_pool.h"
#include "parser.h"
#include "display_ast.h"
#include "stages.h"

int main(int argc, char *argv[]) {
    Args args = arg_parse(argc, argv);

    FILE *input_file, *output_file;
    if ((input_file = fopen(args.input, "r")) == NULL) {
        fprintf(stderr, "error: nonexistent input file (%s)\n", args.input);
        exit(EXIT_FAILURE);
    }

    StrPool strs = { 0 };
    Parser parser = { 0 };

    char out_filename[256];

    if (strcmp(args.target, "scan") == 0) {
        yyscan_t scanner;
        if (yylex_init_extra(&strs, &scanner)) {
            return 1;
        }
        yyset_in(input_file, scanner);

        sprintf(out_filename, "%s.lex", args.input);
        if (( output_file = fopen(out_filename, "w")) == NULL) {
            fprintf(stderr, "error: could not create output file\n");
            exit(EXIT_FAILURE);
        }

        scanner_stage(scanner, strs, output_file);

        yylex_destroy(scanner);
    } else if (strcmp(args.target, "parse") == 0) {
        yyscan_t scanner;
        if (yylex_init_extra(&strs, &scanner)) {
            return 1;
        }
        yyset_in(input_file, scanner);
        if (yyparse(&parser, scanner)) {
            return 1;
        }
        yylex_destroy(scanner);

        Ast ast = Parser_mk_ast(&parser);

        sprintf(out_filename, "%s.sint", args.input);
        if ((output_file = fopen(out_filename, "w")) == NULL) {
            fprintf(stderr, "error: could not create output file\n");
            exit(EXIT_FAILURE);
        }
        ast_display(ast, 0, strs, output_file);

        Ast_release(&ast);
    } else {
        fprintf(stderr, "error: unimplemented stage");
        exit(EXIT_FAILURE);
    }

    StrPool_release(&strs);
    fclose(input_file);
    fclose(output_file);
    arg_release(&args);

    return 0;
}
