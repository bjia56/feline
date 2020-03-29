%{ open Ast %}

%token PLS GIV HAI ME TEH FUNC WIT KTHXBAI QUESTION
%token <string> IDENT
%token <int> INTLIT
%token <string> STRLIT
%token MEOW WIT

%start expr
%type <Ast.expr> expr
%%

expr:
    | IDENT { Ident($1) }

FUNCTCALL:
	| MEOW WIT args

args:
	| expr
	| expr args
 
STRLIT:
	| QUOTATION  QUOTATION
	| QUOTATION id QUOTATION
