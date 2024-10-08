%{
#include "str_pool.h"
#include "_parser.h"

#define YY_EXTRA_TYPE StrPool *
%}

%option outfile="src/lexer.c" header-file="include/_lexer.h"

%option noyywrap nounput noinput nounistd noyylineno nostack
%option warn reentrant never-interactive

%option bison-bridge bison-locations

WS      [ \t]
NL      \r\n|\n\r|\n 
NUMBER  [0-9]+
IDENT   [a-zA-Z][a-zA-Z0-9_]*

%x ML_COMMENT

%%
    uint32_t comment_nesting = 0;
%{
    yylloc->col += yyleng;
%}

    /* Whitespace */
{WS}+    { yylloc->col += yyleng; }
{NL}+    { yylloc->col = 1; yylloc->line += yyleng; }
"//".*\n { yylloc->col = 1; yylloc->line += 1; }
"/*"     { yylloc->col += yyleng; BEGIN(ML_COMMENT); }

<ML_COMMENT>{
  "/*"      { yylloc->col += yyleng; comment_nesting++; }
  "*"+"/"   { yylloc->col += yyleng;
              if (comment_nesting) { comment_nesting--; }
              else { BEGIN(INITIAL); } }
  "*"+      { yylloc->col += yyleng; }
  [^/*\n]+  { yylloc->col += yyleng; }
  [/]       { yylloc->col += yyleng; }
  {NL}+     { yylloc->col = 1; yylloc->line += yyleng; }
}

    /* Keywords */
"bool"      { return TOK_BOOL; }
"else"      { return TOK_ELSE; }
"extern"    { return TOK_EXTERN; }
"false"     { return TOK_FALSE; }
"if"        { return TOK_IF; }
"integer"   { return TOK_INTEGER; }
"program"   { return TOK_PROGRAM; }
"return"    { return TOK_RETURN; }
"then"      { return TOK_THEN; }
"true"      { return TOK_TRUE; }
"void"      { return TOK_VOID; }
"while"     { return TOK_WHILE; }
    /* Operators */
"-"     { return TOK_MINUS; }
"!"     { return TOK_BANG; }
"*"     { return TOK_STAR; }
"/"     { return TOK_SLASH; }
"%"     { return TOK_MOD; }
"+"     { return TOK_PLUS; }
"<"     { return TOK_LT; }
">"     { return TOK_GT; }
"=="    { return TOK_EQEQ; }
"&&"    { return TOK_AMPAMP; }
"||"    { return TOK_BARBAR; }
"="     { return TOK_EQ; }
    /* Separators */
"("     { return TOK_LPAREN; }
")"     { return TOK_RPAREN; }
"{"     { return TOK_LCURLY; }
"}"     { return TOK_RCURLY; }
";"     { return TOK_SEMICOLON; }
","     { return TOK_COMMA; }

{NUMBER}  { yylval->TOK_NUMBER = atoll(yytext); return TOK_NUMBER; }
{IDENT}   { yylval->TOK_IDENT = StrPool_put(yyextra, yytext); return TOK_IDENT; }

.       { return TOK_ILLEGAL_CHAR; }

%%
