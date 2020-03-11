%{ open Ast %}

%token PLS GIV HAI ME TEH FUNC WIT KTHXBAI QUESTION
%token <string> IDENT
%token <int> INTLIT
%token <string> STRLIT

%start expr
%type <Ast.expr> expr
%%

expr:
    | IDENT { Ident($1) }
