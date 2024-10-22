#include <gtest/gtest.h>

#include "parser.h"
#include "ast.h"
#include "str_pool.h"
#include "tyck.h"

class SempassTest : public ::testing::Test {
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

    bool TypeChecks(const Ast ast) {
        return tyck(ast, str_pool);
    }

    AstNode Root(Ast ast) {
        return ast.nodes[0];
    }
};

TEST_F(SempassTest, EmptyProgram) {
    Ast ast;
    ASSERT_EQ(ParseString("program {}", ast), 0);
    ASSERT_TRUE(TypeChecks(ast));
}

TEST_F(SempassTest, MultiDeclaration) {
    Ast ast;
    ASSERT_EQ(
        ParseString(
            "program {\n"
            "   integer hola() extern;\n"
            "   integer hola(integer x) extern;\n"
            "}",
            ast),
        0);
    ASSERT_TRUE(!TypeChecks(ast));

    ASSERT_EQ(
        ParseString(
            "program {\n"
            "   bool a = true;\n"
            "   bool a = false;\n"
            "}",
            ast),
        0);
    ASSERT_TRUE(!TypeChecks(ast));
}

TEST_F(SempassTest, TypeMismatch) {
    Ast ast;
    ASSERT_EQ(
        ParseString(
            "program {\n"
            "   bool qux = 2 * 1;\n"
            "}",
            ast),
        0);
    ASSERT_TRUE(!TypeChecks(ast));

    ASSERT_EQ(
        ParseString(
            "program {\n"
            "   integer qux = false;\n"
            "}",
            ast),
        0);
    ASSERT_TRUE(!TypeChecks(ast));
}

TEST_F(SempassTest, MethodAssign) {
    Ast ast;
    ASSERT_EQ(
        ParseString(
            "program {\n"
            "   integer hola() { return 10; }\n"

            "   void main() {\n"
            "       hola = 15;\n"
            "   }\n"
            "}",
            ast),
        0);
    ASSERT_TRUE(!TypeChecks(ast));

    ASSERT_EQ(
        ParseString(
            "program {\n"
            "   integer hola() extern;\n"

            "   void main() {\n"
            "       hola = 15;\n"
            "   }\n"
            "}",
            ast),
        0);
    ASSERT_TRUE(!TypeChecks(ast));
}

TEST_F(SempassTest, MethodArithmetic) {
    Ast ast;
    ASSERT_EQ(
        ParseString(
            "program {\n"
            "   integer hola() extern;\n"

            "   void main() {\n"
            "       integer hola_mas_1 = hola + 1;\n"
            "   }\n"
            "}",
            ast),
        0);
    ASSERT_TRUE(!TypeChecks(ast));
}

TEST_F(SempassTest, BadFunctionCall) {
    Ast ast;
    ASSERT_EQ(
        ParseString(
            "program {\n"

            "   integer hola0() extern;\n"
            "   integer hola1(integer i) extern;\n"
            "   integer hola2(integer i, integer j) extern;\n"

            "   void main() {\n"
            "       hola0(10, 11);\n"
            "       hola2(15);\n"
            "       hola1();\n"
            "   }\n"
            "}",
            ast),
        0);
    ASSERT_TRUE(!TypeChecks(ast));
}

TEST_F(SempassTest, SampleProgram) {
    Ast ast;
    ASSERT_EQ(
        ParseString(
            "program {\n"

            "    integer getint() extern;\n"
            "    integer printint(integer i) extern;\n"
            "    integer dec(integer i) extern;\n"
            "    integer inc(integer i) extern;\n"

            "    void main() {\n"
            "        integer y = 0;\n"
            "        y = getint();\n"

            "        if (y == 1) then {\n"
            "            inc(y);\n"
            "        } else {\n"
            "            dec(y);\n"
            "        }\n"

            "       printint(y);\n"
            "    }\n"
            "}\n",
            ast),
        0);
    ASSERT_TRUE(TypeChecks(ast));
}
