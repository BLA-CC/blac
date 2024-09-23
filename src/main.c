#include <stdio.h>
#include "str_pool.h"
#include "parser.h"
#include "lexer.h"

int main(int argc, char *argv[]) {
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
        switch (n->kind) {
        case AstNodeKind_PROG:
            printf("{%u; %u}\n", n->data.PROG.vars, n->data.PROG.meths);
            break;
        case AstNodeKind_VAR_DECL:
            printf("%u %s = %u\n", n->data.VAR_DECL.type, StrPool_get(&strs, n->data.VAR_DECL.ident), n->data.VAR_DECL.expr);
            break;
        case AstNodeKind_METH_DECL:
            printf("%u %s :: %u\n", n->data.METH_DECL.proto, StrPool_get(&strs, n->data.METH_DECL.ident), n->data.METH_DECL.body);
            break;
        case AstNodeKind_METH_PROTO:
            printf("%u -> %u\n", n->data.METH_PROTO.params, n->data.METH_PROTO.ret_type);
            break;
        case AstNodeKind_PARAM:
            printf("%u %s\n", n->data.PARAM.type, StrPool_get(&strs, n->data.PARAM.ident));
            break;
        case AstNodeKind_ASGN:
            printf("%s = %u\n", StrPool_get(&strs, n->data.ASGN.var_ident), n->data.ASGN.expr);
            break;
        case AstNodeKind_IF:
            printf("%u ? %u : %u\n", n->data.IF.cond, n->data.IF.then_b, n->data.IF.else_b);
            break;
        case AstNodeKind_WHILE:
            printf("while (%u): %u\n", n->data.WHILE.cond, n->data.WHILE.body);
            break;
        case AstNodeKind_RET:
            printf("ret %u\n", n->data.RET.ret_val);
            break;
        case AstNodeKind_BLOCK:
            printf("{%u; %u}\n", n->data.PROG.vars, n->data.PROG.meths);
            break;
        case AstNodeKind_NOP:
            printf("nop\n");
            break;
        case AstNodeKind_METH_CALL:
            printf("%s (%u)\n", StrPool_get(&strs, n->data.METH_CALL.meth_ident), n->data.METH_CALL.args);
            break;
        case AstNodeKind_VAR:
            printf("%s\n", StrPool_get(&strs, n->data.VAR.ident));
            break;
        case AstNodeKind_INT_LIT:
            printf("i%zd\n", n->data.INT_LIT.lit_val);
            break;
        case AstNodeKind_BOOL_LIT:
            printf("b%d\n", n->data.BOOL_LIT.lit_val);
            break;
        case AstNodeKind_UNOP:
            printf("<%u> %u\n", n->data.UNOP.op, n->data.UNOP.arg);
            break;
        case AstNodeKind_BINOP:
            printf("%u <%u> %u\n", n->data.BINOP.lhs, n->data.BINOP.op, n->data.BINOP.rhs);
            break;
        case AstNodeKind_LIST:
            printf("%u .. %u\n", n->data.LIST.begin, n->data.LIST.end);
            break;
        }
    }

    return 0;
}
