#include "parser.h"
#include "str_pool.h"
#include <stdio.h>


void scanner_stage(yyscan_t scanner, StrPool strs, FILE *file) {
    YYSTYPE yylval;
    YYLTYPE yylloc = {.col = 1, .line = 1};

    int token;
    while ((token = yylex(&yylval, &yylloc, scanner)) != 0) {
        switch (token) {
            case TOK_BOOL:
                fprintf(file, "TOK_BOOL(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_ELSE:
                fprintf(file, "TOK_ELSE(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_EXTERN:
                fprintf(file, "TOK_EXTERN(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_FALSE:
                fprintf(file, "TOK_FALSE(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_IF:
                fprintf(file, "TOK_IF(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_INTEGER:
                fprintf(file, "TOK_INTEGER(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_PROGRAM:
                fprintf(file, "TOK_PROGRAM(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_RETURN:
                fprintf(file, "TOK_RETURN(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_THEN:
                fprintf(file, "TOK_THEN(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_TRUE:
                fprintf(file, "TOK_TRUE(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_VOID:
                fprintf(file, "TOK_VOID(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_WHILE:
                fprintf(file, "TOK_WHILE(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_MINUS:
                fprintf(file, "TOK_MINUS(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_BANG:
                fprintf(file, "TOK_BANG(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_STAR:
                fprintf(file, "TOK_STAR(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_SLASH:
                fprintf(file, "TOK_SLASH(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_MOD:
                fprintf(file, "TOK_MOD(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_PLUS:
                fprintf(file, "TOK_PLUS(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_LT:
                fprintf(file, "TOK_LT(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_GT:
                fprintf(file, "TOK_GT(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_EQEQ:
                fprintf(file, "TOK_EQEQ(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_AMPAMP:
                fprintf(file, "TOK_AMPAMP(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_BARBAR:
                fprintf(file, "TOK_BARBAR(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_EQ:
                fprintf(file, "TOK_EQ(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_LPAREN:
                fprintf(file, "TOK_LPAREN(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_RPAREN:
                fprintf(file, "TOK_RPAREN(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_LCURLY:
                fprintf(file, "TOK_LCURLY(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_RCURLY:
                fprintf(file, "TOK_RCURLY(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_SEMICOLON:
                fprintf(file, "TOK_SEMICOLON(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_COMMA:
                fprintf(file, "TOK_COMMA(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            case TOK_NUMBER:
                fprintf(file, "TOK_NUMBER(=%d, %u:%u)\n",
                yylval.TOK_NUMBER, yylloc.line, yylloc.col);
                break;
            case TOK_IDENT:
                fprintf(
                    file, "TOK_IDENT(=%s, %u:%u)\n",
                    StrPool_get(&strs, yylval.TOK_IDENT),
                    yylloc.line, yylloc.col
                );
                break;
            case TOK_ILLEGAL_CHAR:
                fprintf(file, "TOK_ILLEGAL_CHAR(%u:%u)\n", yylloc.line, yylloc.col);
                break;
            default:
                fprintf(file, "UNKNOWN_TOKEN(%u:%u)\n", yylloc.line, yylloc.col);
                break;
        }
    }
}
