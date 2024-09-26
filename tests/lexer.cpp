#include <gtest/gtest.h>

extern "C" {
#include "parser.h"
#include "lexer.h"
}

#include "str_pool.h"

class LexerTest : public ::testing::Test {
  protected:
    StrPool str_pool = { 0 };
    yyscan_t scanner;
    YY_BUFFER_STATE buffer;

    void SetUp() override { yylex_init_extra(&str_pool, &scanner); }

    void TearDown() override {
        yy_delete_buffer(buffer, scanner);
        yylex_destroy(scanner);
        StrPool_release(&str_pool);
    }

    void setTokens(const char *tokens) {
        buffer = yy_scan_string(tokens, scanner);
    }

    int getNextToken(YYSTYPE *yylval, YYLTYPE *yylloc) {
        return yylex(yylval, yylloc, scanner);
    }

    void assertTokens(const std::vector<int> &expected_tokens) {
        YYSTYPE yylval;
        YYLTYPE yylloc;
        int token;
        for (size_t i = 0; i < expected_tokens.size(); i++) {
            token = getNextToken(&yylval, &yylloc);
            ASSERT_EQ(token, expected_tokens[i])
                << "Token mismatch at index " << i;
        }
        ASSERT_EQ(getNextToken(&yylval, &yylloc), YYEOF);
    }
};

TEST_F(LexerTest, CaseSensitive) {
    setTokens("WHILE cont Cont");

    YYSTYPE yylval_l, yylval_u;
    YYLTYPE yylloc;

    ASSERT_NE(getNextToken(&yylval_l, &yylloc), TOK_WHILE);

    getNextToken(&yylval_l, &yylloc); // "cont"
    getNextToken(&yylval_u, &yylloc); // "Cont"

    ASSERT_NE(
        StrPool_get(&str_pool, yylval_l.TOK_IDENT),
        StrPool_get(&str_pool, yylval_u.TOK_IDENT));
}

TEST_F(LexerTest, Keywords) {
    setTokens("extern bool program else then false if integer return true void "
              "while");

    std::vector<int> expected_tokens = { TOK_EXTERN, TOK_BOOL,    TOK_PROGRAM,
                                         TOK_ELSE,   TOK_THEN,    TOK_FALSE,
                                         TOK_IF,     TOK_INTEGER, TOK_RETURN,
                                         TOK_TRUE,   TOK_VOID,    TOK_WHILE };

    assertTokens(expected_tokens);
}

TEST_F(LexerTest, Comments) {
    setTokens("// one line comment\nreturn\n/*several\nlines "
              "comment\n*/\nwhile/*/*hola*/*/if");

    std::vector<int> expected_tokens = { TOK_RETURN, TOK_WHILE, TOK_IF };

    assertTokens(expected_tokens);
}

TEST_F(LexerTest, Whitespaces) {
    setTokens("while  /*comment! true*/    true\n{\n// comment!\n    "
              "return/*comment!*/;}");

    std::vector<int> expected_tokens = {
        TOK_WHILE, TOK_TRUE, TOK_LCURLY, TOK_RETURN, TOK_SEMICOLON, TOK_RCURLY
    };

    assertTokens(expected_tokens);
}

TEST_F(LexerTest, ConcatenatedTokens) {
    setTokens("whiletrue");

    YYSTYPE yylval;
    YYLTYPE yylloc;

    ASSERT_EQ(getNextToken(&yylval, &yylloc), TOK_IDENT);
    ASSERT_EQ(yylval.TOK_IDENT, StrPool_put(&str_pool, "whiletrue"));
    ASSERT_EQ(getNextToken(&yylval, &yylloc), YYEOF);
}

TEST_F(LexerTest, Literals) {
    setTokens("2147483647 -2147483648 18446744073709551616");

    YYSTYPE yylval;
    YYLTYPE yylloc;

    ASSERT_EQ(getNextToken(&yylval, &yylloc), TOK_NUMBER);
    ASSERT_EQ(yylval.TOK_NUMBER, 2147483647);

    ASSERT_EQ(getNextToken(&yylval, &yylloc), TOK_MINUS);
    ASSERT_EQ(getNextToken(&yylval, &yylloc), TOK_NUMBER);
    ASSERT_EQ(yylval.TOK_NUMBER, 2147483648);
    // ASSERT_EQ(getNextToken(&yylval, &yylloc), TOK_ILLEGAL);
}

TEST_F(LexerTest, Identifiers) {
    setTokens("234aa ?_. th1s1s0k");

    std::vector<int> expected_tokens = { TOK_NUMBER,       TOK_IDENT,
                                         TOK_ILLEGAL_CHAR, TOK_ILLEGAL_CHAR,
                                         TOK_ILLEGAL_CHAR, TOK_IDENT };

    assertTokens(expected_tokens);
}
