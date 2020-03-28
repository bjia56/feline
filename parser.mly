%{ open Ast %}

%token I HAS A VARBL ITZ GIVEZ PLS GIV HAI ME TEH FUNC WIT KTHXBAI QUESTION NEWLINE
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT GT AND OR NOT
%token <string> IDENT
%token <int> INTEGR
%token <bool> BLIT  
%token <string> STRIN

%start expr
%type <Ast.expr> expr

%right ITZ
%left OR
%left AND
%left NOT
%left EQ NEQ
%left LT GT
%left DIVIDE TIMES
%left PLUS MINUS

%%

expr:
      INTEGR                          { IntLit($1)         }
    | BLIT			      { BoolLit($1)        } 
    | STRIN                           { StrLit($1)         }
    | IDENT                           { Ident($1)          }
    | expr PLUS expr		      { Binop($1, Add, $3) } 
    | expr MINUS expr 		      { Binop($1, Sub, $3) }                   | expr TIMES expr                 { Binop($1, Mul, $3) }
    | expr DIVIDE expr                { Binop($1, Div, $3) }
    | expr NEQ expr 		      { Binop($1, Neq, $3) }
    | expr EQ expr		      { Binop($1, Eq, $3)  }
    | expr LT expr		      { Binop($1, Less, $3) }
    | expr GT expr		      { Binop($1, Greater, $3) }
    | expr AND expr		      { Binop($1, And, $3) }
    | expr OR expr		      { Binop($1, Or, $3) }
    | NOT expr			      { Unop(Not, $2)           }
    | IDENT ITZ expr                  { Assign($1, $3)     }

typ:
    | INTEGR { Int }
    | STRIN  { String }
    | IDENT  { Ident($1) }

func_decl:
    HAI ME TEH typ FUNC IDENT formals_opt NEWLINE stmt_list KTHXBAI  
    {
      {
        rtyp=$4;
        fname=$6;
        formals=$7;
        body=$9
      }
    }

formals_opt:
  /*nothing*/ { [] }
  | WIT formals_list { $2 }

formals_list:
  | formal { [$1] }
  | formal formals_list { $1 :: $2 }

formal:
  | IDENT TEH typ { ($3, $1) } 

vdecl:
  | I HAS A VARBL IDENT TEH typ { ($7, $5) }
  
stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list { $1::$2 }
      
stmt:
  vdecl ITZ expr NEWLINE { Seq($1, $3) }
  | vdecl NEWLINE       { Bind $1 }
  | expr NEWLINE        { Expr $1 }
  | GIVEZ expr NEWLINE  { Return $2 }
  | NEWLINE             { Blank }
