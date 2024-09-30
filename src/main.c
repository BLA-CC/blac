#include <stdio.h>

#include "str_pool.h"
#include "parser.h"

int main(int argc, char *argv[]) {
    (void) argc; (void) argv;

    StrPool strs = {0};
    Parser parser = {0};

    yyscan_t scanner;
    if (yylex_init_extra(&strs, &scanner)) {
        return 1;
    }
    if (yyparse(&parser, scanner)) {
        return 1;
    }

    for (uint32_t i = 0; i < parser.nodes.len; i++) {
        AstNode *n = &parser.nodes.elems[i];
        printf("(%3u %3u) %3u. ", n->loc.line, n->loc.col, i);
        printf("%u %u %u\n", n->kind, n->data.lhs, n->data.rhs);
    }

    return 0;
}
