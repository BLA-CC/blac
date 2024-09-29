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
        switch (n->kind) {
        case AstNodeKind_PROG:
            printf("{%u; %u}\n", n->data.PROG.begin, n->data.PROG.end);
            break;
        case AstNodeKind_VAR_INIT:
            printf("%u := %u\n", n->data.VAR_INIT.decl, n->data.VAR_INIT.init_expr);
            break;
        case AstNodeKind_VAR_DECL:
            printf("var %u %s\n", n->data.VAR_DECL.type, StrPool_get(&strs, n->data.VAR_DECL.ident));
            break;
        case AstNodeKind_METH_IMPL:
            printf("%u :: %u\n", n->data.METH_IMPL.decl, n->data.METH_IMPL.body);
            break;
        case AstNodeKind_METH_DECL:
            printf("meth %u %s\n", n->data.METH_DECL.proto, StrPool_get(&strs, n->data.METH_DECL.ident));
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
        case AstNodeKind_IF_SMP:
            printf("%u ? %u\n", n->data.IF_SMP.cond, n->data.IF_SMP.branch);
            break;
        case AstNodeKind_IF_ALT:
            printf("%u ? %u : %u\n", n->data.IF_ALT.cond, n->data.IF_ALT.branches, n->data.IF_ALT.branches + 1);
            break;
        case AstNodeKind_WHILE:
            printf("while (%u): %u\n", n->data.WHILE.cond, n->data.WHILE.body);
            break;
        case AstNodeKind_RET:
            printf("ret %u\n", n->data.RET.ret_val);
            break;
        case AstNodeKind_BLOCK:
            printf("{%u; %u}\n", n->data.BLOCK.begin, n->data.BLOCK.end);
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
        case AstNodeKind_NEG:
            printf("! %u\n", n->data.UNM.arg);
            break;
        case AstNodeKind_MUL:
            printf("%u * %u\n", n->data.MUL.lhs, n->data.MUL.rhs);
            break;
        case AstNodeKind_DIV:
            printf("%u / %u\n", n->data.DIV.lhs, n->data.DIV.rhs);
            break;
        case AstNodeKind_MOD:
            printf("%u %% %u\n", n->data.MOD.lhs, n->data.MOD.rhs);
            break;
        case AstNodeKind_ADD:
            printf("%u + %u\n", n->data.ADD.lhs, n->data.ADD.rhs);
            break;
        case AstNodeKind_SUB:
            printf("%u - %u\n", n->data.SUB.lhs, n->data.SUB.rhs);
            break;
        case AstNodeKind_LT:
            printf("%u < %u\n", n->data.LT.lhs, n->data.LT.rhs);
            break;
        case AstNodeKind_GT:
            printf("%u > %u\n", n->data.GT.lhs, n->data.GT.rhs);
            break;
        case AstNodeKind_EQ:
            printf("%u == %u\n", n->data.EQ.lhs, n->data.EQ.rhs);
            break;
        case AstNodeKind_AND:
            printf("%u && %u\n", n->data.AND.lhs, n->data.AND.rhs);
            break;
        case AstNodeKind_OR:
            printf("%u || %u\n", n->data.OR.lhs, n->data.OR.rhs);
            break;
        case AstNodeKind_LIST:
            printf("%u .. %u\n", n->data.LIST.begin, n->data.LIST.end);
            break;
        }
    }

    return 0;
}
