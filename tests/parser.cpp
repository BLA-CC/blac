#include <gtest/gtest.h>

#include "parser.h"
#include "str_pool.h"
#include "common.h"

class ParserTest : public ::testing::Test {
  protected:
    StrPool str_pool = { 0 };
    yyscan_t scanner;
    YY_BUFFER_STATE buffer;
    Parser parser = { 0 };

    void SetUp() override { yylex_init_extra(&str_pool, &scanner); }

    void TearDown() override {
        yylex_destroy(scanner);
        StrPool_release(&str_pool);
    }

    int ParseString(const char *input) {
        YY_BUFFER_STATE buffer = yy_scan_string(input, scanner);
        int result = yyparse(&parser, scanner);
        yy_delete_buffer(buffer, scanner);
        return result;
    }

    AstNode Root() { return parser.nodes.elems[0]; }

    AstNode Node(NodeIdx idx) { return parser.nodes.elems[idx]; }

    int AstLen() { return parser.nodes.len; }

    AstNode FirstMethod() {
        return Node(Node(Root().data.PROG.meths).data.LIST.begin);
    }

    AstNode FirstVarDecl(AstNode meth) {
        return Node(Node(Node(meth.data.METH_DECL.body).data.BLOCK.vars)
                        .data.LIST.begin);
    }

    void AssertVarDecl(const AstNode &var_decl, Type type, const char *name) {
        ASSERT_EQ(var_decl.data.VAR_DECL.type, type);
        ASSERT_EQ(var_decl.data.VAR_DECL.ident, StrPool_put(&str_pool, name));
    }

    void AssertBinOp(
        const AstNode &node,
        BinOp op,
        AstNodeKind lhs_kind,
        AstNodeKind rhs_kind) {
        ASSERT_EQ(node.kind, AstNodeKind_BINOP);
        ASSERT_EQ(node.data.BINOP.op, op);
        ASSERT_EQ(Node(node.data.BINOP.lhs).kind, lhs_kind);
        ASSERT_EQ(Node(node.data.BINOP.rhs).kind, rhs_kind);
    }
};

TEST_F(ParserTest, ParseProgram) {
    ASSERT_NE(ParseString("integer x = 42;"), 0);
    ASSERT_EQ(ParseString("program{}"), 0);
    ASSERT_EQ(AstLen(), 1);
    ASSERT_EQ(Root().kind, AstNodeKind_PROG);
}

TEST_F(ParserTest, ExampleProgram) {
    ASSERT_EQ(
        ParseString("program {\n"
                    "    void main() {\n"
                    "        integer y = 0;\n"
                    "        y = getint();\n"
                    "        if (y == 1) then {\n"
                    "            printint(y);\n"
                    "        } else {\n"
                    "            return printint(inc(y));\n"
                    "        }\n"
                    "    }\n"
                    "}\n"),
        0);
}

TEST_F(ParserTest, ProgramBody) {
    ASSERT_EQ(
        ParseString(" program {\n"
                    "     integer ivar = 0;\n"
                    "     bool bvar = 0;\n"
                    "     integer method(integer param){ return; }\n"
                    "     void external() extern;\n"
                    " }\n"),
        0);
    ASSERT_EQ(AstLen(), 16);

    ASSERT_EQ(Root().kind, AstNodeKind_PROG);

    AstNode vars = Node(Root().data.PROG.vars);
    AstNode meths = Node(Root().data.PROG.meths);

    for (NodeIdx i = vars.data.LIST.begin; i < vars.data.LIST.end; i++) {
        ASSERT_EQ(Node(i).kind, AstNodeKind_VAR_DECL);
    }

    for (NodeIdx i = meths.data.LIST.begin; i < meths.data.LIST.end; i++) {
        ASSERT_TRUE(
            (Node(i).kind == AstNodeKind_METH_DECL) ||
            (Node(i).kind == AstNodeKind_METH_PROTO));
    }

    ASSERT_NE(
        ParseString(" program {\n"
                    "     void method(){ }\n"
                    "     bool bvar = 0;\n"
                    " }\n"),
        0);
}

TEST_F(ParserTest, ParseVariableDecl) {
    ASSERT_EQ(
        ParseString(" program {\n"
                    "     bool bvar = true;\n"
                    "     integer ivar = 0;\n"
                    " }\n"),
        0);

    ASSERT_EQ(AstLen(), 6);

    AstNode decl_node = Node(Node(Root().data.PROG.vars).data.LIST.begin);
    AssertVarDecl(decl_node, Type_BOOL, "bvar");
    ASSERT_EQ(Node(decl_node.data.VAR_DECL.expr).kind, AstNodeKind_BOOL_LIT);

    ASSERT_NE(ParseString("program { bool bvar; }"), 0);
    ASSERT_NE(ParseString("program { bool; }"), 0);
}

TEST_F(ParserTest, ParseMethodDecl) {
    ASSERT_EQ(
        ParseString(
            " program {\n"
            "     void main(integer a, bool b) { integer foo = 2; return; }\n"
            "     integer func1(integer a) { integer foo = 2; return; }\n"
            "     bool func2() { return true; }\n"
            "     void print(integer i) extern;\n"
            " }\n"),
        0);
    ASSERT_EQ(AstLen(), 33);
    AstNode main = FirstMethod();

    ASSERT_EQ(main.data.METH_DECL.ident, StrPool_put(&str_pool, "main"));
    ASSERT_EQ(Node(main.data.METH_DECL.body).kind, AstNodeKind_BLOCK);

    AstNode main_proto = Node(main.data.METH_DECL.proto);
    ASSERT_EQ(main_proto.data.METH_PROTO.ret_type, Type_VOID);

    AstNode params = Node(main_proto.data.METH_PROTO.params);
    AssertVarDecl(Node(params.data.LIST.begin), Type_INT, "a");
    AssertVarDecl(Node(params.data.LIST.end - 1), Type_BOOL, "b");
}

TEST_F(ParserTest, ParseBlock) {
    ASSERT_EQ(
        ParseString(" program {\n"
                    "     void main() {\n"
                    "         integer x = 10;\n"
                    "         bool w = false;\n"
                    "         x = true;\n"
                    "         w = true;\n"
                    "     }\n"
                    " }\n"),
        0);

    ASSERT_NE(
        ParseString(" program {\n"
                    "     void main() {\n"
                    "         integer x = 10;\n"
                    "         x = true;\n"
                    "         bool w = false;\n"
                    "         w = true;\n"
                    "     }\n"
                    " }\n"),
        0);

    ASSERT_NE(
        ParseString(" program {\n"
                    "     void main() {\n"
                    "         void func(integer a) {}\n"
                    "     }\n"
                    " }\n"),
        0);
}

TEST_F(ParserTest, ParseStatements) {
    ASSERT_EQ(
        ParseString(" program {\n"
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
                    " }\n"),
        0);

    AstNode stmts =
        Node(Node(FirstMethod().data.METH_DECL.body).data.BLOCK.stmts);

    ASSERT_EQ(stmts.kind, AstNodeKind_LIST);
    ASSERT_EQ(stmts.data.LIST.end - stmts.data.LIST.begin, 5);

    AstNode assign_stmt = Node(stmts.data.LIST.begin);
    ASSERT_EQ(assign_stmt.kind, AstNodeKind_ASGN);

    AstNode if_stmt = Node(stmts.data.LIST.begin + 2);
    ASSERT_EQ(if_stmt.kind, AstNodeKind_IF);
}

TEST_F(ParserTest, ParseArithExpression) {
    ASSERT_EQ(ParseString("program { void f() { integer x = 1 + 2 * 3; }}"), 0);

    AstNode var_decl = FirstVarDecl(FirstMethod());
    AssertVarDecl(var_decl, Type_INT, "x");

    AstNode expr = Node(var_decl.data.VAR_DECL.expr);
    AssertBinOp(expr, BinOp_ADD, AstNodeKind_INT_LIT, AstNodeKind_BINOP);

    AstNode right = Node(expr.data.BINOP.rhs);
    AssertBinOp(right, BinOp_MUL, AstNodeKind_INT_LIT, AstNodeKind_INT_LIT);

    ASSERT_NE(ParseString("program { void f() { integer x = 10 + ; }}"), 0);
    ASSERT_NE(ParseString("program { void f() { integer x = ; }}"), 0);
}

TEST_F(ParserTest, ParseBoolExpressions) {
    ASSERT_EQ(
        ParseString(" program {\n"
                    "     void main() {\n"
                    "         bool result = true && false;\n"
                    "     }\n"
                    " }\n"),
        0);

    AstNode decl = FirstVarDecl(FirstMethod());
    AssertVarDecl(decl, Type_BOOL, "result");

    AstNode expr = Node(decl.data.VAR_DECL.expr);
    AssertBinOp(expr, BinOp_AND, AstNodeKind_BOOL_LIT, AstNodeKind_BOOL_LIT);
}
