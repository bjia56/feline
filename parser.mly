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
    HAI ME TEH typ FUNC IDENT formals_opt stmt_list KTHXBAI  
    {
      {
        rtyp=$4;
        fname=$6;
        formals_opt=$7;
        body=$8
      }
    }

formals_opt:
  /*nothing*/ { [] }
  | WIT formals_list { $2 }

formals_list:
  | formal { [$1] }
  | formal formals_list { $1 :: $3 }

formal:
  | IDENT TEH typ { ($3, $1) } 

vdecl:
  | I HAS A IDENT TEH typ { ($6, $4) }
  
stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list { $1::$2 }
      
stmt:
  vdecl ASSIGN expr NEWLINE { Seq($1, $3) }
  | vdecl NEWLINE       { Bind $1 }
  | expr NEWLINE        { Expr $1 }
  | GIVEZ expr NEWLINE  { Return $2 }
