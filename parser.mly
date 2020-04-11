%{ open Ast %}

%token I HAS A VARBL ITZ GIVEZ PLS GIV HAI ME TEH FUNC MEOW WIT KTHXBAI QUESTION NEWLINE
%token NOT_SAYM_AZ SAYM_AZ
%token BIGGR_THAN SMALLR_THAN
%token PLUZ MYNUZ TYMEZ DIVYD
%token AN OR OPOZIT
%token <string> IDENT
%token <int> INTEGR
%token <bool> BLIT
%token <string> STRIN
%token BUL
%token EOF

%start program
%type <Ast.program> program

%left OR AN
%left OPOZIT
%left BIGGR_THAN SMALLR_THAN SAYM_AZ NOT_SAYM_AZ
%left PLUZ MYNUZ
%left DIVYD TYMEZ

%%

program:
    | decls EOF         { $1 }
    | decls NEWLINE EOF { $1 }

decls:
    | /* nothing */     { { classes=[]; functions=[]; globals=[] } }
    | func_decl decls   { { classes=$2.classes; functions=$1::$2.functions; globals=$2.globals } }
    | glob_vdecl decls  { { classes=$2.classes; functions=$2.functions; globals=$1::$2.globals } }

/*FUNCTCALL:
	| MEOW WIT args

args:
	| expr
	| expr args
 
  STRLIT:
	| QUOTATION  QUOTATION
	| QUOTATION id QUOTATION*/
  
expr:
    | INTEGR                { IntLit($1) }
    | BLIT                  { BoolLit($1) }
    | STRIN                 { StrLit($1) }
    | IDENT                 { Ident($1) }
    | expr PLUZ expr        { Binop($1, Add, $3) }
    | expr MYNUZ expr       { Binop($1, Sub, $3) }
    | expr TYMEZ expr       { Binop($1, Mul, $3) }
    | expr DIVYD expr       { Binop($1, Div, $3) }
    | expr NOT_SAYM_AZ expr { Binop($1, Neq, $3) }
    | expr SAYM_AZ expr     { Binop($1, Eq, $3) }
    | expr SMALLR_THAN expr { Binop($1, Less, $3) }
    | expr BIGGR_THAN expr  { Binop($1, Greater, $3) }
    | expr AN expr          { Binop($1, And, $3) }
    | expr OR expr          { Binop($1, Or, $3) }
    | OPOZIT expr           { Unop(Not, $2) }

typ:
    | INTEGR { Int }
    | STRIN  { String }
    | IDENT  { TypIdent($1) }
    | BUL    { Bool }

func_decl:
    | HAI ME TEH typ FUNC IDENT formals_opt NEWLINE stmt_list KTHXBAI
        {
            {
                rtyp=$4;
                fname=$6;
                formals=$7;
                body=$9;
            }
        }
    | HAI ME TEH FUNC IDENT formals_opt NEWLINE stmt_list KTHXBAI
        {
            {
                rtyp=Null;
                fname=$5;
                formals=$6;
                body=$8;
            }
        }

formals_opt:
    | /*nothing*/ { [] }
    | WIT formals_list { $2 }

formals_list:
    | formal { [$1] }
    | formal AN WIT formals_list { $1 :: $4 }

formal:
    | IDENT TEH typ { ($3, $1) }

glob_vdecl:
    | HAI ME TEH VARBL IDENT TEH typ { ($7, $5) }

loc_vdecl:
    | I HAS A VARBL IDENT TEH typ { ($7, $5) }

stmt_list:
    | /* nothing */ { [] }
    | stmt stmt_list { $1::$2 }

stmt:
    | loc_vdecl ITZ expr NEWLINE { BindAssign($1, $3) }
    | IDENT ITZ expr NEWLINE { Assign($1, $3) }
    | loc_vdecl NEWLINE       { Bind $1 }
    | expr NEWLINE        { Expr $1 }
    | GIVEZ expr NEWLINE  { Return $2 }