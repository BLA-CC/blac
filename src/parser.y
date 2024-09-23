 /******************************* Parser specs *******************************/
%define api.pure
%define api.value.type union

%parse-param { Parser *parser }
%param { void *scanner }

%define parse.error verbose

/***************************** File Organization *****************************/
%output  "src/parser.c"
%defines "include/parser.h"

%define api.header.include {"parser.h"}

%code requires {
    #include "ast.h"

    typedef struct {
        AstNodeVec nodes;
        AstNodeVec scratch;
    } Parser;
}

%code provides {
    void yyerror(Location *loc, void *scanner, Parser *parser, const char *msg);
    #define YY_DECL \
        int yylex(YYSTYPE* yylval_param, Location* yylloc_param , void *yyscanner)
    extern YY_DECL;
}

/***************************** Location Tracking *****************************/
%locations
%define api.location.type {Location}

%initial-action {
    @$.line = 1;
    @$.col = 1;
}

%{
    # define YYLLOC_DEFAULT(Cur, Rhs, N)        \
    do                                          \
        if (N) {                                \
            (Cur).line = YYRHSLOC(Rhs, 1).line; \
            (Cur).col = YYRHSLOC(Rhs, 1).col;   \
        } else {                                \
            (Cur).line = YYRHSLOC(Rhs, 0).line; \
            (Cur).col = YYRHSLOC(Rhs, 0).col;   \
        }                                       \
    while (0)
%}

%code {
    #define nodes parser->nodes
    #define scratch parser->scratch
    #define nodes_push(x) AstNodeVec_push(&nodes, x)
    #define scratch_push(x) AstNodeVec_push(&scratch, x)
    #define mk_list(b) AstBuilder_mk_list(&nodes, &scratch, b)
}

%token
    TOK_BOOL        "bool"
    TOK_ELSE        "else"
    TOK_EXTERN      "extern"
    TOK_FALSE       "false"
    TOK_IF          "if"
    TOK_INTEGER     "integer"
    TOK_PROGRAM     "program"
    TOK_RETURN      "return"
    TOK_THEN        "then"
    TOK_TRUE        "true"
    TOK_VOID        "void"
    TOK_WHILE       "while"
    TOK_MINUS       "-"
    TOK_BANG        "!"
    TOK_STAR        "*"
    TOK_SLASH       "/"
    TOK_MOD         "%"
    TOK_PLUS        "+"
    TOK_LT          "<"
    TOK_GT          ">"
    TOK_EQEQ        "=="
    TOK_AMPAMP      "&&"
    TOK_BARBAR      "||"
    TOK_EQ          "="
    TOK_LPAREN      "("
    TOK_RPAREN      ")"
    TOK_LCURLY      "{"
    TOK_RCURLY      "}"
    TOK_SEMICOLON   ";"
    TOK_COMMA       ","
  <uint32_t>
    TOK_NUMBER      "number"
    TOK_IDENT       "identifier"
%token TOK_ILLEGAL_CHAR

%type <AstNode>
    var_decl
    meth_decl
    param
    block
    stmt
    meth_call
    expr
%type <NodeIdx>
    meth_impl.opt
    expr.opt
    else.opt
%type <Type>
    type
    ret_type

%left "||"
%left "&&"
%left "=="
%left "<" ">"
%left "+" "-"
%left "*" "/" "%"
%precedence NEG // logic negation
%precedence UNM // unary minus

%%
program:
    "program"
    { nodes_push(AstNode_mk(@1, PROG)); }
    "{"
    { $<uint32_t>$ = scratch.len; }
    var_decl.list
    {
        $<uint32_t>$ = mk_list($<uint32_t>4);
        // scratch.len gets resetted, so is now $4 again
    }
    meth_decl.list
    { $<uint32_t>$ = mk_list($<uint32_t>4); }
    "}"
    {
        AstNode *root = &nodes.elems[0];
        root->data.PROG.vars = $<uint32_t>6;
        root->data.PROG.meths = $<uint32_t>8;
        return yynerrs;
    };

var_decl: type TOK_IDENT "=" expr ";" {
        nodes_push($4);
        $$ = AstNode_mk(@2, VAR_DECL, $1, $2, nodes.len - 1);
    };

meth_decl:
    ret_type TOK_IDENT "("
    { $<uint32_t>$ = scratch.len; }
    param.list
    { $<uint32_t>$ = mk_list($<uint32_t>4); }
    ")" meth_impl.opt {
        AstNode proto = AstNode_mk(@1, METH_PROTO, $<uint32_t>6, $1);
        nodes_push(proto);
        $$ = AstNode_mk(@1, METH_DECL, $2, nodes.len - 1, $8);
    };

stmt: TOK_IDENT "=" expr ";" {
        nodes_push($3);
        $$ = AstNode_mk(@2, ASGN, $1, nodes.len - 1);
    }
    | meth_call ";" { $$ = $1; }
    | "if" "(" expr ")" "then" block else.opt ";" {
        nodes_push($3); nodes_push($6);
        $$ = AstNode_mk(@1, IF, nodes.len - 2, nodes.len - 1, nodes.len - 3);
    }
    | "while" "(" expr ")" block ";" {
        nodes_push($3); nodes_push($5);
        $$ = AstNode_mk(@1, WHILE, nodes.len - 2, nodes.len - 1);
    }
    | "return" expr.opt ";" { $$ = AstNode_mk(@1, RET, $2); }
    | ";" { $$ = AstNode_mk(@1, NOP); }
    | block { $$ = $1; }
    ;

expr: meth_call { $$ = $1; }
    | "(" expr ")" { $$ = $2; }
    | TOK_IDENT { $$ = AstNode_mk(@1, VAR, $1); }
    | TOK_NUMBER { $$ = AstNode_mk(@1, INT_LIT, $1); }
    | TOK_TRUE { $$ = AstNode_mk(@1, BOOL_LIT, true); }
    | TOK_FALSE { $$ = AstNode_mk(@1, BOOL_LIT, false); }
    | "-" expr %prec UNM {
        nodes_push($2);
        $$ = AstNode_mk(@1, UNOP, nodes.len - 1, UnOp_UNM);
    }
    | "!" expr %prec NEG {
        nodes_push($2);
        $$ = AstNode_mk(@1, UNOP, nodes.len - 1, UnOp_NEG);
    }
    | expr "*" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, BINOP, nodes.len - 2, nodes.len - 1, BinOp_MUL);
    }
    | expr "/" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, BINOP, nodes.len - 2, nodes.len - 1, BinOp_DIV);
    }
    | expr "%" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, BINOP, nodes.len - 2, nodes.len - 1, BinOp_MOD);
    }
    | expr "+" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, BINOP, nodes.len - 2, nodes.len - 1, BinOp_ADD);
    }
    | expr "-" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, BINOP, nodes.len - 2, nodes.len - 1, BinOp_SUB);
    }
    | expr "<" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, BINOP, nodes.len - 2, nodes.len - 1, BinOp_LT);
    }
    | expr ">" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, BINOP, nodes.len - 2, nodes.len - 1, BinOp_GT);
    }
    | expr "==" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, BINOP, nodes.len - 2, nodes.len - 1, BinOp_EQ);
    }
    | expr "&&" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, BINOP, nodes.len - 2, nodes.len - 1, BinOp_AND);
    }
    | expr "||" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, BINOP, nodes.len - 2, nodes.len - 1, BinOp_OR);
    }
    ;

block:
    "{"
    { $<uint32_t>$ = scratch.len; }
    var_decl.list
    {
        $<uint32_t>$ = mk_list($<uint32_t>2);
        // list_nodes.len is resetted, so is now $2 again
    }
    stmt.list
    { $<uint32_t>$ = mk_list($<uint32_t>2); }
    "}"
    { $$ = AstNode_mk(@1, BLOCK, $<uint32_t>4, $<uint32_t>6); };

meth_call:
    TOK_IDENT "("
    { $<uint32_t>$ = scratch.len; }
    arg.list
    { $<uint32_t>$ = mk_list($<uint32_t>3); }
    ")" { $$ = AstNode_mk(@1, METH_CALL, $1, $<uint32_t>5); };

param: type TOK_IDENT { $$ = AstNode_mk(@1, PARAM, $1, $2); };

type: "integer" { $$ = Type_INT; }
    | "bool"    { $$ = Type_BOOL; }
    ;
ret_type
    : type      { $$ = $1; }
    | "void"    { $$ = Type_VOID; }
    ;

// LISTS (built but not finished)

arg.list
    :
    | arg.list1
    ;

arg.list1
    : expr { scratch_push($1); }
    | arg.list1 "," expr { scratch_push($3); }
    ;

param.list
    :
    | param.list1
    ;

param.list1
    : param { scratch_push($1); }
    | param.list1 "," param { scratch_push($3); }
    ;

stmt.list
    :
    | stmt.list stmt { scratch_push($2); }
    ;

var_decl.list
    :
    | var_decl.list var_decl { scratch_push($2); }
    ;

meth_decl.list
    :
    | meth_decl.list meth_decl { scratch_push($2); }
    ;

// OPTIONALS (pushed eagerly)

meth_impl.opt
    : "extern" ";" { $$ = NO_NODE; }
    | block { nodes_push($1); $$ = nodes.len - 1; }
    ;

expr.opt
    : { $$ = NO_NODE; }
    | expr { nodes_push($1); $$ = nodes.len - 1; }
    ;

else.opt
    : { $$ = NO_NODE; }
    | "else" block { nodes_push($2); $$ = nodes.len - 1; }
    ;

%%

#include <stdio.h>

void yyerror(YYLTYPE *loc, void* scanner, Parser *parser, const char *msg) {
    (void) scanner, (void) parser;
    fprintf(stderr, "%d:%d: %s\n", loc->line, loc->col, msg);
}
