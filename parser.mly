%{ open Ast %}

%token PLS GIV HAI ME TEH FUNC WIT KTHXBAI QUESTION
%token <string> IDENT
%token <int> INTLIT
%token <string> STRLIT

%start expr
%type <Ast.expr> expr

%start func
%type <Ast.func> func
%%

expr:
    | IDENT { Ident($1) }

typ:
    | INTEGR { Int }
    | STRIN  { String }
    | IDENT  { Ident($1) }

func_decl:
    HAI ME TEH typ FUNC IDENT vdecl_list stmt_list KTHXBAI  
    {
      {
        rtyp=typ;
        fname=$6;
        formals=vdecl_list
        locals=$7
        body=$8
      }
    }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl vdecl_list { $1 :: $3 }

vdecl:
  I HAS A IDENT TEH typ { ($6, $4) }
  
stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list { $1::$2 }
      
stmt:
  | expr        { Expr $1 }
  | GIVEZ expr  { Return $2 }
