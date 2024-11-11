#include <gtest/gtest.h>

#include "parser.h"
#include "ast.h"
#include "str_pool.h"
#include "common.h"

class ParserTest : public ::testing::Test {
  protected:
    StrPool str_pool;
    yyscan_t scanner;
    YY_BUFFER_STATE buffer;
    Parser parser;

    void SetUp() override {
        str_pool = { 0 };
        yylex_init_extra(&str_pool, &scanner);
    }

    void TearDown() override {
        yylex_destroy(scanner);
    }

    int ParseString(const char *input, Ast &ast) {
        parser = { { 0 } };
        YY_BUFFER_STATE buffer = yy_scan_string(input, scanner);
        int result = yyparse(&parser, scanner);

        if (result != 0) {
            return result;
        }

        ast = Parser_mk_ast(&parser);
        yy_delete_buffer(buffer, scanner);
        return result;
    }

    AstNode Root(Ast ast) {
        return ast.nodes[0];
    }
};

TEST_F(ParserTest, ParseProgram) {
    Ast ast;
    ASSERT_NE(ParseString("integer a = 4;", ast), 0);
    ASSERT_EQ(ParseString("program{}", ast), 0);
    ASSERT_EQ(ast.len, 1);
    ASSERT_EQ(Root(ast).kind, AstNodeKind_PROG);
}

TEST_F(ParserTest, ExampleProgram) {
    Ast ast;
    ASSERT_EQ(
        ParseString(
            "program {\n"
            "    void main() {\n"
            "        integer y = 0;\n"
            "        y = getint();\n"
            "        if (y == 1) then {\n"
            "            printint(y);\n"
            "        } else {\n"
            "            return printint(inc(y));\n"
            "        }\n"
            "    }\n"
            "}\n",
            ast),
        0);
}

TEST_F(ParserTest, ProgramBody) {
    Ast ast;
    ASSERT_EQ(
        ParseString(
            " program {\n"
            "     integer ivar = 0;\n"
            "     integer method(integer param){ return; }\n"
            "     bool bvar = 0;\n"
            "     void external() extern;\n"
            " }\n",
            ast),
        0);
    ASSERT_EQ(ast.len, 17);

    AstNodeFull_List prog = Ast_full_prog(ast);
    ASSERT_EQ(prog.end - prog.begin, 4);
}

TEST_F(ParserTest, ParseVariableDecl) {
    Ast ast;
    ASSERT_EQ(
        ParseString(
            " program {\n"
            "     bool bvar = true;\n"
            "     integer ivar = 0;\n"
            " }\n",
            ast),
        0);

    ASSERT_EQ(ast.len, 7);

    AstNodeFull_VarDecl var_decl =
        Ast_full_var_decl(ast, Ast_full_prog(ast).begin);
    ASSERT_EQ(var_decl.type, Type_BOOL);
    ASSERT_EQ(var_decl.ident, StrPool_put(&str_pool, "bvar"));
    ASSERT_NE(var_decl.init_expr, NO_NODE);

    ASSERT_NE(ParseString("program { bool bvar; }", ast), 0);
    ASSERT_NE(ParseString("program { bool; }", ast), 0);
}

TEST_F(ParserTest, ParseMethodDecl) {
    Ast ast;
    ASSERT_EQ(
        ParseString(
            " program {\n"
            "     void main(integer a, bool b) { integer foo = 2; return; }\n"
            "     integer func1(integer a) { integer foo = 2; return; }\n"
            "     bool func2() { return true; }\n"
            "     void print(integer i) extern;\n"
            " }\n",
            ast),
        0);

    ASSERT_EQ(ast.len, 33);
    AstNodeFull_MethDecl meth =
        Ast_full_meth_decl(ast, Ast_full_prog(ast).begin);

    ASSERT_EQ(meth.ident, StrPool_put(&str_pool, "main"));
    ASSERT_EQ(meth.ret_type, Type_VOID);

    NodeIdx params_begin = ast.nodes[meth.params].data.lhs;

    ASSERT_EQ(ast.nodes[params_begin].data.lhs, Type_INT);

    ASSERT_EQ(ast.nodes[params_begin].data.rhs, StrPool_put(&str_pool, "a"));

    ASSERT_EQ(ast.nodes[params_begin + 1].data.lhs, Type_BOOL);

    ASSERT_EQ(
        ast.nodes[params_begin + 1].data.rhs, StrPool_put(&str_pool, "b"));
}

TEST_F(ParserTest, ParseBlock) {
    Ast ast;
    ASSERT_EQ(
        ParseString(
            " program {\n"
            "     void main() {\n"
            "         integer x = 10;\n"
            "         bool w = false;\n"
            "         x = true;\n"
            "         w = true;\n"
            "     }\n"
            " }\n",
            ast),
        0);

    ASSERT_NE(
        ParseString(
            " program {\n"
            "     void main() {\n"
            "         void func(integer a) {}\n"
            "     }\n"
            " }\n",
            ast),
        0);
}

TEST_F(ParserTest, ParseStatements) {
    Ast ast;
    ASSERT_EQ(
        ParseString(
            " program {\n"
            "     void main() {\n"
            "         integer x = 0;\n"
            "         integer y = 100 + 32;\n"
            "         x = 10;\n"
            "         x = func(x, y);\n"
            "         if (x == 10*y) then {\n"
            "             x = 5;\n"
            "         }\n"
            "         while (x < 10) {\n"
            "             x = x + 1;\n"
            "         }\n"
            "         return 0;\n"
            "     }\n"
            " }\n",
            ast),
        0);

    AstNodeFull_List body = Ast_full_block(
        ast, Ast_full_meth_decl(ast, Ast_full_prog(ast).begin).body);
    ASSERT_EQ(body.end - body.begin, 7);
}

TEST_F(ParserTest, ParseArithExpression) {
    Ast ast;
    ASSERT_EQ(ParseString("program { integer x = 1 + 2 * 3; }", ast), 0);

    AstNodeFull_VarDecl var_decl =
        Ast_full_var_decl(ast, Ast_full_prog(ast).begin);
    ASSERT_EQ(var_decl.ident, StrPool_put(&str_pool, "x"));
    ASSERT_EQ(var_decl.type, Type_INT);

    AstNode *binop = &ast.nodes[var_decl.init_expr];
    ASSERT_EQ(binop->kind, AstNodeKind_ADD);
    AstNode *rhs = &ast.nodes[binop->data.rhs];
    ASSERT_EQ(rhs->kind, AstNodeKind_MUL);

    ASSERT_NE(
        ParseString("program { void f() { integer x = 10 + ; }}", ast), 0);
    ASSERT_NE(ParseString("program { void f() { integer x = ; }}", ast), 0);
}

TEST_F(ParserTest, ParseBoolExpressions) {
    Ast ast;
    ASSERT_EQ(
        ParseString(" program { bool result = true && false; }\n", ast), 0);

    AstNodeFull_VarDecl var_decl =
        Ast_full_var_decl(ast, Ast_full_prog(ast).begin);
    AstNode *binop = &ast.nodes[var_decl.init_expr];
    ASSERT_EQ(binop->kind, AstNodeKind_AND);
}
