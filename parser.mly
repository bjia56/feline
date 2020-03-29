%{ open Ast %}

%token I HAS A VARBL ITZ GIVEZ PLS GIV HAI ME TEH FUNC WIT NOT SAYM AZ BIGGR SMALLR THAN KTHXBAI QUESTION NEWLINE
%token PLUZ MYNUZ TYMEZ DIVYD
%token AN OR OPOZIT
%token <string> IDENT
%token <int> INTEGR
%token <bool> BLIT
%token <string> STRIN
%token EOF

%start program
%type <Ast.program> program

%right ITZ
%left OR AN
%left OPOZIT
%left BIGGR SMALLR SAYM
%left DIVYD TYMEZ
%left PLUZ MYNUZ

%%

program:
    | decls EOF { $1 }

decls:
    | /* nothing */   { ([], [] ) }
    | func_decl decls { (fst $2, ($1 :: snd $2)) }

expr:
    | INTEGR                { IntLit($1) }
    | BLIT                  { BoolLit($1) }
    | STRIN                 { StrLit($1) }
    | IDENT                 { Ident($1) }
    | expr PLUZ expr        { Binop($1, Add, $3) }
    | expr MYNUZ expr       { Binop($1, Sub, $3) }
    | expr TYMEZ expr       { Binop($1, Mul, $3) }
    | expr DIVYD expr       { Binop($1, Div, $3) }
    | expr NOT SAYM AZ expr { Binop($1, Neq, $5) }
    | expr SAYM AZ expr     { Binop($1, Eq, $4) }
    | expr SMALLR THAN expr { Binop($1, Less, $4) }
    | expr BIGGR THAN expr  { Binop($1, Greater, $4) }
    | expr AN expr          { Binop($1, And, $3) }
    | expr OR expr          { Binop($1, Or, $3) }
    | NOT expr              { Unop(Not, $2) }

typ:
    | INTEGR { Int }
    | STRIN  { String }
    | IDENT  { TypIdent($1) }

func_decl:
    | HAI ME TEH typ FUNC IDENT formals_opt NEWLINE stmt_list KTHXBAI
        {
            {
                rtyp=$4;
                fname=$6;
                formals=$7;
                body=$9
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

vdecl:
    | I HAS A VARBL IDENT TEH typ { ($7, $5) }

stmt_list:
    | /* nothing */ { [] }
    | stmt stmt_list { $1::$2 }

stmt:
    | vdecl ITZ expr NEWLINE { BindAssign($1, $3) }
    | IDENT ITZ expr NEWLINE { Assign($1, $3) }
    | vdecl NEWLINE       { Bind $1 }
    | expr NEWLINE        { Expr $1 }
    | GIVEZ expr NEWLINE  { Return $2 }
    | NEWLINE             { Blank }
