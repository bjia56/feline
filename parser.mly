%{
open Ast
%}

%token I HAS A VARBL ITZ GIVEZ PLS GIV HAI ME TEH FUNC CLAS DIS NU DELET CONS DES WIT IN KTHXBAI KTHX EVRYONE MESELF NEWLINE
%token INTEGR BUL NOL IF ELS WYL
%token NOT_SAYM_AZ SAYM_AZ
%token BIGGR_THAN SMALLR_THAN
%token PLUZ MYNUZ TYMEZ DIVYD
%token AN OR OPOZIT
%token <string> IDENT
%token <string> IDENT_QUESTION
%token <int> INTLIT
%token <bool> BLIT
%token <string> STRLIT
%token EOF

%start module_decl
%type <Ast.module_decl> module_decl

%left OR AN
%left OPOZIT
%left BIGGR_THAN SMALLR_THAN SAYM_AZ NOT_SAYM_AZ
%left PLUZ MYNUZ
%left DIVYD TYMEZ

%%

module_decl:
    | imports_and_decls EOF { $1 }

imports_and_decls:
    | imports NEWLINE imports_and_decls { { imports=$1 @ $3.imports; classes=$3.classes; functions=$3.functions; globals=$3.globals } }
    | decls                             { $1 }

decls:
    | /* nothing */            { { imports=[]; classes=[]; functions=[]; globals=[] } }
    | func_decl NEWLINE decls  { { imports=[]; classes=$3.classes; functions=$1::$3.functions; globals=$3.globals } }
    | glob_vdecl NEWLINE decls { { imports=[]; classes=$3.classes; functions=$3.functions; globals=$1::$3.globals } }
    | class_decl NEWLINE decls { { imports=[]; classes=$1::$3.classes; functions=$3.functions; globals=$3.globals } }
    | NEWLINE decls            { $2 }

imports:
    | PLS GIV imports_list { $3 }

imports_list:
    | IDENT_QUESTION              { [$1] }
    | IDENT_QUESTION imports_list { $1 :: $2 }

expr:
    | NOL                   { NullLit }
    | INTLIT                { IntLit($1) }
    | BLIT                  { BoolLit($1) }
    | STRLIT                { StrLit($1) }
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
    | IDENT IN functcall    { let id, args = $3 in ClassFunctcall(id, ($1, args)) }
    | IDENT IN IDENT        { ClassMemAccess($1, $3) }
    | IDENT IN DIS          { ClassMemAccess($1, "DIS") }
    | NU IDENT              { NewInstance($2) }

typ:
    | INTEGR { Int }
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
                rtyp=Void;
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
    | /* nothing */     { [] }
    | stmt stmt_list    { $1::$2 }
    | NEWLINE stmt_list { $2 }

stmt:
    | loc_vdecl ITZ expr NEWLINE                     { BindAssign($1, $3) }
    | IDENT ITZ expr NEWLINE                         { Assign($1, $3) }
    | loc_vdecl NEWLINE                              { Bind($1) }
    | expr NEWLINE                                   { Expr($1) }
    | GIVEZ expr NEWLINE                             { Return($2) }
    | IF expr NEWLINE stmt_list KTHX NEWLINE                 { If($2, $4)}
    | IF expr NEWLINE stmt_list ELS stmt_list KTHX NEWLINE   { IfElse($2, $4, $6) }
    | WYL expr NEWLINE stmt_list KTHX NEWLINE                { While($2, $4) }
    | IDENT IN IDENT ITZ expr NEWLINE                { ClassMemRassn($1, $3, $5) }
    | IDENT IN DIS ITZ expr NEWLINE                  { ClassMemRassn($1, "DIS", $5) }
    | DELET expr NEWLINE                             { Dealloc($2) }

functcall:
    | IDENT WIT functcall_args KTHX { ($1, $3) }
    | DIS WIT functcall_args KTHX   { ("DIS", $3 ) }

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
    | EVRYONE NEWLINE class_pub_internals class_internals
        {
            {
                cname="";
                pubmembers=$3.pubmembers @ $4.pubmembers;
                privmembers=$4.privmembers;
                pubfuncs=$3.pubfuncs @ $4.pubfuncs;
                privfuncs=$4.privfuncs;
                cons=$3.cons @ $4.cons;
                des=$3.des @ $4.des;
            }
        }
    | MESELF NEWLINE class_priv_internals class_internals
        {
            {
                cname="";
                pubmembers=$4.pubmembers;
                privmembers=$3.privmembers @ $4.privmembers;
                pubfuncs=$4.pubfuncs;
                privfuncs=$3.privfuncs @ $4.privfuncs;
                cons=$4.cons;
                des=$4.des;
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
                pubmembers=$3.pubmembers;
                privmembers=[];
                pubfuncs=$1::$3.pubfuncs;
                privfuncs=[];
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
    | DIS TEH typ FUNC IDENT formals_opt NEWLINE stmt_list KTHXBAI
        {
            {
                rtyp=$3;
                fname=$5;
                formals=$6;
                body=$8;
            }
        }
    | DIS TEH FUNC IDENT formals_opt NEWLINE stmt_list KTHXBAI
        {
            {
                rtyp=Void;
                fname=$4;
                formals=$5;
                body=$7;
            }
        }

class_vdecl:
    | DIS TEH VARBL IDENT TEH typ { ($6, $4) }

cons_decl:
    | DIS TEH CONS formals_opt NEWLINE stmt_list KTHXBAI { $6 }

des_decl:
    | DIS TEH DES NEWLINE stmt_list KTHXBAI { $5 }
