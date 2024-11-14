#include <gtest/gtest.h>
#include <stdexcept>
#include <string>
#include <cstdio>

#include "parser.h"
#include "ast.h"
#include "str_pool.h"
#include "tyck.h"
#include "ir.h"
extern "C" {
#include "display.h"
}
#include "ir.h"

class IrGenTest : public ::testing::Test {
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

    int ParseAndCheck(const char *input, Ast &ast) {
        parser = { { 0 } };
        YY_BUFFER_STATE buffer = yy_scan_string(input, scanner);
        int result = yyparse(&parser, scanner);

        if (result != 0) {
            return result;
        }

        ast = Parser_mk_ast(&parser);
        yy_delete_buffer(buffer, scanner);

        return tyck(ast, str_pool);
    }

    Ir IrGen(Ast &ast) {
        return mk_ir(ast);
    }

    AstNode Root(Ast ast) {
        return ast.nodes[0];
    }

    std::string CaptureIrDisplay(Ir &ir, StrPool &str_pool, int indent) {
        char* buffer = nullptr;
        size_t size = 0;

        FILE* stream = open_memstream(&buffer, &size);
        if (!stream) {
            throw std::runtime_error("Failed to open memory stream");
        }

        display_ir(ir, str_pool, indent, stream);
        fflush(stream);
        fclose(stream);
        std::string result(buffer);
        free(buffer);

        return result;
    }
};

TEST_F(IrGenTest, SimpleProgram) {
    Ast ast;

    ASSERT_EQ(ParseAndCheck("program { void main(){return;}}", ast), 1);
    Ir ir = IrGen(ast);

    std::string actualOutput = CaptureIrDisplay(ir, str_pool, 0);

    std::string expectedOutput =
        "G := {\n}\n\n"
        "main (0 params, 0 locals)\n"
        "    ret $0\n\n";

    EXPECT_EQ(actualOutput, expectedOutput);
}

TEST_F(IrGenTest, Globals) {
    Ast ast;

    ASSERT_EQ(ParseAndCheck(
        "program {\n"
        "    integer int_global = 34 * 9 - 2;\n"
        "    bool bool_global = (true && false) || true;\n"
        "    integer main() {\n"
        "        bool a = bool_global;\n"
        "        return int_global;\n"
        "    }\n"
        "}\n", ast), 1);
    Ir ir = IrGen(ast);

    std::string actualOutput = CaptureIrDisplay(ir, str_pool, 0);

    std::string expectedOutput =
        "G := {\n"
        "  int_global: $304\n"
        "  bool_global: $1\n"
        "}\n\n"
        "main (0 params, 2 locals)\n"
        "    t1 := G[bool_global]\n"
        "    t2 := G[int_global]\n"
        "    ret t2\n\n";

    EXPECT_EQ(actualOutput, expectedOutput);
}

TEST_F(IrGenTest, MethCall) {
    Ast ast;

    ASSERT_EQ(ParseAndCheck(
        "program {\n"
        "    bool foo(integer a, integer b) {\n"
        "        return true;\n"
        "    }\n"
        "    bool main() {\n"
        "        return foo(12, 4);\n"
        "    }\n"
        "}\n", ast), 1);
    Ir ir = IrGen(ast);

    std::string actualOutput = CaptureIrDisplay(ir, str_pool, 0);

    std::string expectedOutput =
        "G := {\n}\n\n"
        "foo (2 params, 2 locals)\n"
        "    ret $1\n\n"
        "main (0 params, 1 locals)\n"
        "    arg[1] := $4\n"
        "    arg[0] := $12\n"
        "    t1 := call foo 2\n"
        "    ret t1\n\n";

    EXPECT_EQ(actualOutput, expectedOutput);
}

TEST_F(IrGenTest, MethCallEvalInOrder) {
    Ast ast;

    ASSERT_EQ(ParseAndCheck(
        "program {\n"
        "    integer foo(integer a1, integer a2, integer a3) {\n"
        "        return 0;\n"
        "    }\n"
        "    void main() {\n"
        "        foo(1+2, 3*4, 5/6);\n"
        "    }\n"
        "}", ast), 1);
    Ir ir = IrGen(ast);

    std::string actualOutput = CaptureIrDisplay(ir, str_pool, 0);

    std::string expectedOutput =
        "G := {\n}\n\n"
        "foo (3 params, 3 locals)\n"
        "    ret $0\n\n"
        "main (0 params, 0 locals)\n"
        "    arg[2] := $0\n"
        "    arg[1] := $12\n"
        "    arg[0] := $3\n"
        "    t0 := call foo 3\n"
        "    ret $0\n\n";

    EXPECT_EQ(actualOutput, expectedOutput);
}


TEST_F(IrGenTest, ArithExpr) {
    Ast ast;

    ASSERT_EQ(ParseAndCheck(
        "program {\n"
        "    void main() {\n"
        "        integer b = 5;\n"
        "        integer a = ((1 + 2)*3)/4 % b;\n"
        "    }\n"
        "}\n", ast), 1);
    Ir ir = IrGen(ast);

    std::string actualOutput = CaptureIrDisplay(ir, str_pool, 0);

    std::string expectedOutput =
        "G := {\n}\n\n"
        "main (0 params, 3 locals)\n"
        "    t1 := $5\n"
        "    t3 := $2\n"
        "    t2 := mod t3 t1\n"
        "    ret $0\n\n";

    EXPECT_EQ(actualOutput, expectedOutput);
}

TEST_F(IrGenTest, IfElse) {
    Ast ast;

    ASSERT_EQ(ParseAndCheck(
        "program {\n"
        "    void print(integer n) extern;\n"
        "    void main() {\n"
        "        bool x = true;"
        "        if (x) then {\n"
        "            print(0);\n"
        "        } else {\n"
        "            print(1);\n"
        "        }\n"
        "    }\n"
        "}\n", ast), 1);
    Ir ir = IrGen(ast);

    std::string actualOutput = CaptureIrDisplay(ir, str_pool, 0);

    std::string expectedOutput =
        "G := {\n}\n\n"
        
        "main (0 params, 1 locals)\n"
        "    t1 := $1\n"
        "    cmp t1 $0\n"
        "    if flags == eq then jmp .L1\n"
        "  .L0\n"
        "    arg[0] := $0\n"
        "    t0 := call print 1\n"
        "    jmp .L2\n"
        "  .L1\n"
        "    arg[0] := $1\n"
        "    t0 := call print 1\n"
        "  .L2\n"
        "    ret $0\n\n";
    EXPECT_EQ(actualOutput, expectedOutput);
}

TEST_F(IrGenTest, While) {
    Ast ast;

    ASSERT_EQ(ParseAndCheck(
        "program {\n"
        "    void print(integer n) extern;\n"
        "    void main() {\n"
        "        integer a = 0;\n"
        "        while (a < 10) {\n"
        "            a = a + 1;\n"
        "            print(a);\n"
        "        }\n"
        "    }\n"
        "}\n", ast), 1);
    Ir ir = IrGen(ast);

    std::string actualOutput = CaptureIrDisplay(ir, str_pool, 0);

    std::string expectedOutput =
        "G := {\n}\n\n"
        "main (0 params, 1 locals)\n"
        "    t1 := $0\n"
        "  .L0\n"
        "    cmp t1 $10\n"
        "    if flags == ge then jmp .L2\n"
        "  .L1\n"
        "    t1 := add t1 $1\n"
        "    arg[0] := t1\n"
        "    t0 := call print 1\n"
        "    jmp .L0\n"
        "  .L2\n"
        "    ret $0\n\n";

    EXPECT_EQ(actualOutput, expectedOutput);
}

TEST_F(IrGenTest, ProgramExample) {
    Ast ast;

    ASSERT_EQ(ParseAndCheck(
        "program\n"
        "{\n"
        "    integer inc(integer x) {\n"
        "        return x + 1;\n"
        "    }\n"
        "    integer get_int() extern;\n"
        "    void print_int(integer i) extern ;\n"
        "    void main() {\n"
        "        integer y = 0;\n"
        "        y = get_int();\n"
        "        if (y == 1) then {\n"
        "            print_int(y);\n"
        "        } else {\n"
        "            return print_int(inc(y));\n"
        "        }\n"
        "    }\n"
        "}\n", ast), 1);
    Ir ir = IrGen(ast);

    std::string actualOutput = CaptureIrDisplay(ir, str_pool, 0);

    std::string expectedOutput =
        "G := {\n}\n\n"
        "inc (1 params, 2 locals)\n"
        "    t2 := add t1 $1\n"
        "    ret t2\n\n"
        "main (0 params, 3 locals)\n"
        "    t1 := $0\n"
        "    t1 := call get_int 0\n"
        "    cmp t1 $1\n"
        "    if flags == ne then jmp .L1\n"
        "  .L0\n"
        "    arg[0] := t1\n"
        "    t0 := call print_int 1\n"
        "    jmp .L2\n"
        "  .L1\n"
        "    arg[0] := t1\n"
        "    t3 := call inc 1\n"
        "    arg[0] := t3\n"
        "    t2 := call print_int 1\n"
        "    ret t2\n"
        "  .L2\n"
        "    ret $0\n\n";

    EXPECT_EQ(actualOutput, expectedOutput);
}
