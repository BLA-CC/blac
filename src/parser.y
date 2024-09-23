%{

#include <stdio.h>
#include <stdlib.h>

#include "parser.h"
#include "lexer.h"

int yyerror(char *errormsg);

%}

%code requires {
  typedef void* yyscan_t;
}

%output  "src/parser.c"
%defines "include/parser.h"

%token HI BYE

%%

program:
         hi bye
        ;

hi:
        HI     { printf("Hello World\n");   }
        ;
bye:
        BYE    { printf("Bye World\n"); exit(0); }
         ;

%%

int yywrap(void)
{
     return 0;
}

int yyerror(char *errormsg)
{
      fprintf(stderr, "%s\n", errormsg);
      exit(1);
}

