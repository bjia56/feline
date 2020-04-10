%{
open Ast
open ParserUtils
%}

%token I HAS A VARBL ITZ GIVEZ PLS GIV HAI ME TEH FUNC CLAS CONS DES WIT KTHXBAI KTHX QUESTION COLON EVRYONE MESELF NEWLINE
%token NOT_SAYM_AZ SAYM_AZ
%token BIGGR_THAN SMALLR_THAN
%token PLUZ MYNUZ TYMEZ DIVYD
%token AN OR OPOZIT
%token <string> IDENT
%token <int> INTEGR
%token <bool> BLIT
%token <string> STRIN
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

decls:
    | /* nothing */            { { classes=[]; functions=[] } }
    | func_decl NEWLINE decls  { { classes=$3.classes; functions=$1::$3.functions } }
    | class_decl NEWLINE decls { { classes=$1::$3.classes; functions=$3.functions } }
    | NEWLINE decls            { $2 }

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
    | functcall             { Functcall($1) }

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

vdecl:
    | I HAS A VARBL IDENT TEH typ { ($7, $5) }

stmt_list:
    | /* nothing */     { [] }
    | stmt stmt_list    { $1::$2 }
    | NEWLINE stmt_list { $2 }

stmt:
    | vdecl ITZ expr NEWLINE { BindAssign($1, $3) }
    | IDENT ITZ expr NEWLINE { Assign($1, $3) }
    | vdecl NEWLINE          { Bind($1) }
    | expr NEWLINE           { Expr($1) }
    | GIVEZ expr NEWLINE     { Return($2) }

functcall:
    | IDENT WIT functcall_args KTHX { ($1, $3) }

functcall_args:
    | /* nothing */              { [] }
    | expr                       { [$1] }
    | expr AN WIT functcall_args { $1::$4 }

class_decl:
    | HAI ME TEH CLAS IDENT NEWLINE class_internals KTHXBAI
        {
            {
                cname=$5;
                pubmembers=$7.pubmembers;
                privmembers=$7.privmembers;
                pubfuncs=$7.pubfuncs;
                privfuncs=$7.privfuncs;
                cons=$7.cons;
                des=$7.des;
            }
        }

class_internals:
    | class_pub_internals class_internals
        {
            {
                cname="";
                pubmembers=concat_lists $1.pubmembers $2.pubmembers;
                privmembers=$2.privmembers;
                pubfuncs=concat_lists $1.pubfuncs $2.pubfuncs;
                privfuncs=$2.privfuncs;
                cons=concat_lists $1.cons $2.cons;
                des=concat_lists $1.des $2.des;
            }
        }
    | EVRYONE COLON NEWLINE class_pub_internals class_internals
        {
            {
                cname="";
                pubmembers=concat_lists $4.pubmembers $5.pubmembers;
                privmembers=$5.privmembers;
                pubfuncs=concat_lists $4.pubfuncs $5.pubfuncs;
                privfuncs=$5.privfuncs;
                cons=concat_lists $4.cons $5.cons;
                des=concat_lists $4.des $5.des;
            }
        }
    | MESELF COLON NEWLINE class_priv_internals class_internals
        {
            {
                cname="";
                pubmembers=$5.pubmembers;
                privmembers=concat_lists $4.privmembers $5.privmembers;
                pubfuncs=$5.pubfuncs;
                privfuncs=concat_lists $4.privfuncs $5.privfuncs;
                cons=$5.cons;
                des=$5.des;
            }
        }
    | /* nothing */
        {
            {
                cname="";
                pubmembers=[];
                privmembers=[];
                pubfuncs=[];
                privfuncs=[];
                cons=[];
                des=[];
            }
        }

class_priv_internals:
    | NEWLINE class_priv_internals { $2 }
    | class_func_decl NEWLINE class_priv_internals
        {
            {
                cname="";
                pubmembers=[];
                privmembers=$3.privmembers;
                pubfuncs=[];
                privfuncs=$1::$3.privfuncs;
                cons=[];
                des=[];
            }
        }
    | class_vdecl NEWLINE class_priv_internals
        {
            {
                cname="";
                pubmembers=[];
                privmembers=$1::$3.privmembers;
                pubfuncs=[];
                privfuncs=$3.privfuncs;
                cons=[];
                des=[];
            }
        }
    | /* nothing */
        {
            {
                cname="";
                pubmembers=[];
                privmembers=[];
                pubfuncs=[];
                privfuncs=[];
                cons=[];
                des=[];
            }
        }

class_pub_internals:
    | NEWLINE class_pub_internals { $2 }
    | class_func_decl NEWLINE class_pub_internals
        {
            {
                cname="";
                pubmembers=[];
                privmembers=$3.privmembers;
                pubfuncs=[];
                privfuncs=$1::$3.privfuncs;
                cons=[];
                des=[];
            }
        }
    | class_vdecl NEWLINE class_pub_internals
        {
            {
                cname="";
                pubmembers=$1::$3.pubmembers;
                privmembers=[];
                pubfuncs=$3.pubfuncs;
                privfuncs=[];
                cons=$3.cons;
                des=$3.des;
            }
        }
    | cons_decl NEWLINE class_pub_internals
        {
            {
                cname="";
                pubmembers=$3.pubmembers;
                privmembers=[];
                pubfuncs=$3.pubfuncs;
                privfuncs=[];
                cons=$1::$3.cons;
                des=$3.des;
            }
        }
    | des_decl NEWLINE class_pub_internals
        {
            {
                cname="";
                pubmembers=$3.pubmembers;
                privmembers=[];
                pubfuncs=$3.pubfuncs;
                privfuncs=[];
                cons=$3.cons;
                des=$1::$3.des;
            }
        }
    | /* nothing */
        {
            {
                cname="";
                pubmembers=[];
                privmembers=[];
                pubfuncs=[];
                privfuncs=[];
                cons=[];
                des=[];
            }
        }

class_func_decl:
    | func_decl { $1 }

class_vdecl:
    | HAI ME TEH typ VARBL IDENT { ($4, $6) }

cons_decl:
    | HAI ME TEH CONS formals_opt NEWLINE stmt_list KTHXBAI { ($5, $7) }

des_decl:
    | HAI ME TEH DES NEWLINE stmt_list KTHXBAI { $6 }

