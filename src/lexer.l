%{
#include "parser.h"

int yyerror(char *errormsg);

%}

%option outfile="src/lexer.c" header-file="include/lexer.h" 
%option warn nodefault

%option noyywrap never-interactive nounistd
 
%%

("hi"|"oi")"\n"       { return HI;  }
("tchau"|"bye")"\n"   { return BYE; }
[-[]+.,><]            { return yytext[0]; }
.                     { yyerror("Unknow char");  }

%%

