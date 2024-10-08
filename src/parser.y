 /******************************* Parser specs *******************************/
%define api.pure
%define api.value.type union

%parse-param { Parser *parser }
%param { void *scanner }

%define parse.error verbose

/***************************** File Organization *****************************/
%output  "src/parser.c"
%defines "include/_parser.h"

%define api.header.include {"_parser.h"}

%code requires {
    #include "ast.h"

    typedef struct {
        AstNodeVec nodes;
        AstNodeVec scratch;
    } Parser;

    Ast Parser_mk_ast(Parser *parser);
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

/********************************** Helpers **********************************/
%code {
    #include <string.h>
    #include "common.h"

    #define nodes parser->nodes
    #define scratch parser->scratch
    #define nodes_push(x) AstNodeVec_push(&nodes, x)
    #define scratch_push(x) AstNodeVec_push(&scratch, x)
    #define finish_list(b) Parser_finish_list(parser, b)

    // Appends `scratch[scratch_begin..]` to the end of `nodes`
    // and frees the slice from scratch.
    // returns the start of the appended slice
    // TODO: return and handle NO_NODE if `scratch[scratch_begin..] == []`
    static NodeIdx Parser_finish_list(Parser *parser, uint32_t scratch_begin) {
        uint32_t list_len = scratch.len - scratch_begin;
        if (list_len == 0) {
            return nodes.len;
        }

        AstNodeVec_reserve(&nodes, list_len);
        AstNode *nodes_end = &nodes.elems[nodes.len];

        memcpy(nodes_end,
                &scratch.elems[scratch_begin],
                list_len * sizeof(AstNode));
        nodes.len += list_len;
        scratch.len -= list_len;

        return nodes.len - list_len;
    }

    Ast Parser_mk_ast(Parser *parser) {
        assert(nodes.len > 0);
        AstNode *ast_nodes = realloc(nodes.elems, nodes.len*sizeof(AstNode));
        assert(ast_nodes != NULL);
        Ast result = { ast_nodes, nodes.len };
        AstNodeVec_free(&scratch);
        nodes = (AstNodeVec){0};
        return result;
    }
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

%left "||"
%left "&&"
%left "=="
%left "<" ">"
%left "+" "-"
%left "*" "/" "%"
%precedence NEG // logic negation
%precedence UNM // unary minus

%start program
%type <AstNode>
    block
    meth_call
    expr
%type <NodeIdx>
    meth_impl.opt
    expr.opt
%type <Type> type

%%
program:
    "program" "{"
    {
        nodes_push(AstNode_mk(@1, PROG));
        $<uint32_t>$ = scratch.len;
    }
    toplvl.list "}"
    {
        uint32_t prog_begin = finish_list($<uint32_t>3);
        AstNode *root = &nodes.elems[0];
        root->data.lhs = prog_begin;
        root->data.rhs = nodes.len;
        return yynerrs;
    };

scratch.var_decl: type TOK_IDENT "=" expr ";" {
        nodes_push(AstNode_mk(@2, VAR_DECL, $1, $2));
        nodes_push($4);
        scratch_push(AstNode_mk(@2, VAR_DECL_INIT, nodes.len - 2, nodes.len - 1));
    };

scratch.meth_decl:
    type TOK_IDENT "("
    { $<uint32_t>$ = scratch.len; }
    param.list ")"
    {
        uint32_t params_begin = finish_list($<uint32_t>4);
        nodes_push(AstNode_mk(@3, LIST, params_begin, nodes.len));
        nodes_push(AstNode_mk(@1, METH_PROTO, nodes.len - 1, $1));
        $<uint32_t>$ = nodes.len - 1;
    }
    meth_impl.opt {
        AstNode decl = AstNode_mk(@2, METH_DECL, $2, $<uint32_t>7);
        if ($8 == NO_NODE) {
            scratch_push(decl);
        } else {
            nodes_push(decl);
            scratch_push(AstNode_mk(@1, METH_DECL_IMPL, nodes.len - 1, $8));
        }
    };

scratch.stmt: TOK_IDENT "=" expr ";" {
        nodes_push($3);
        scratch_push(AstNode_mk(@2, ASGN, $1, nodes.len - 1));
    }
    | "if" "(" expr ")" "then" block {
        nodes_push($3); nodes_push($6);
        scratch_push(AstNode_mk(@1, IF_SMP, nodes.len - 2, nodes.len - 1));
    }
    | "if" "(" expr ")" "then" block "else" block {
        nodes_push($3); nodes_push($6); nodes_push($8);
        scratch_push(AstNode_mk(@1, IF_ALT, nodes.len - 3, nodes.len - 2));
    }
    | "while" "(" expr ")" block {
        nodes_push($3); nodes_push($5);
        scratch_push(AstNode_mk(@1, WHILE, nodes.len - 2, nodes.len - 1));
    }
    | "return" expr.opt ";" { scratch_push(AstNode_mk(@1, RET, .lhs=$2)); }
    | ";" { /* scratch_push(AstNode_mk(@1, NOP)); */ }
    | meth_call ";" { scratch_push($1); }
    | block { scratch_push($1); }
    ;

expr: "(" error ")" { yyerrok; }
    | meth_call { $$ = $1; }
    | "(" expr ")" { $$ = $2; }
    | TOK_IDENT { $$ = AstNode_mk(@1, VAR, .lhs=$1); }
    | TOK_NUMBER { $$ = AstNode_mk(@1, INT_LIT, .lhs=$1); }
    | TOK_TRUE { $$ = AstNode_mk(@1, BOOL_LIT, .lhs=true); }
    | TOK_FALSE { $$ = AstNode_mk(@1, BOOL_LIT, .lhs=false); }
    | "-" expr %prec UNM {
        nodes_push($2);
        $$ = AstNode_mk(@1, UNM, .lhs=nodes.len - 1);
    }
    | "!" expr %prec NEG {
        nodes_push($2);
        $$ = AstNode_mk(@1, NEG, .lhs=nodes.len - 1);
    }
    | expr "*" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, MUL, nodes.len - 2, nodes.len - 1);
    }
    | expr "/" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, DIV, nodes.len - 2, nodes.len - 1);
    }
    | expr "%" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, MOD, nodes.len - 2, nodes.len - 1);
    }
    | expr "+" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, ADD, nodes.len - 2, nodes.len - 1);
    }
    | expr "-" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, SUB, nodes.len - 2, nodes.len - 1);
    }
    | expr "<" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, LT, nodes.len - 2, nodes.len - 1);
    }
    | expr ">" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, GT, nodes.len - 2, nodes.len - 1);
    }
    | expr "==" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, EQ, nodes.len - 2, nodes.len - 1);
    }
    | expr "&&" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, AND, nodes.len - 2, nodes.len - 1);
    }
    | expr "||" expr {
        nodes_push($1); nodes_push($3);
        $$ = AstNode_mk(@2, OR, nodes.len - 2, nodes.len - 1);
    }
    ;

block: "{" error "}" { yyerrok; }
    | "{"
    { $<uint32_t>$ = scratch.len; }
    blk.list "}"
    {
        uint32_t block_begin = finish_list($<uint32_t>2);
        $$ = AstNode_mk(@1, BLOCK, block_begin, nodes.len);
    };

meth_call: TOK_IDENT "(" error ")" { yyerrok; } |
    TOK_IDENT "("
    { $<uint32_t>$ = scratch.len; }
    arg.list ")"
    {
        uint32_t args_begin = finish_list($<uint32_t>3);
        nodes_push(AstNode_mk(@2, LIST, args_begin, nodes.len));
        $$ = AstNode_mk(@1, METH_CALL, $1, nodes.len - 1);
    };

scratch.param: type TOK_IDENT { scratch_push(AstNode_mk(@1, PARAM, $1, $2)); };

type: "integer" { $$ = Type_INT; }
    | "bool"    { $$ = Type_BOOL; }
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
    : scratch.param
    | param.list1 "," scratch.param
    ;

toplvl.list
    :
    | toplvl.list scratch.var_decl
    | toplvl.list scratch.meth_decl
    ;

blk.list
    :
    | blk.list scratch.var_decl
    | blk.list scratch.stmt
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

%%

#include <stdio.h>

void yyerror(YYLTYPE *loc, void* scanner, Parser *parser, const char *msg) {
    (void) scanner, (void) parser;
    fprintf(stderr, "%d:%d: %s\n", loc->line, loc->col, msg);
}
