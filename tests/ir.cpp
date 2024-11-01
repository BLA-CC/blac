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
        return mk_ir(ast, str_pool);
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

TEST_F(IrGenTest, Program) {
    Ast ast;

    ASSERT_EQ(ParseAndCheck("program { void main(){return;}}", ast), 1);
    Ir ir = IrGen(ast);

    std::string actualOutput = CaptureIrDisplay(ir, str_pool, 0);

    std::string expectedOutput =
        "main (0 locals)\n"
        "    ret t0\n"
        "    ret t0\n";

    EXPECT_EQ(actualOutput, expectedOutput);
}
