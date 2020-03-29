type typ = Int | String | TypIdent of string

type binop = 
      Add
    | Sub
    | Mul
    | Div
    | Neq
    | Eq
    | Less
    | Greater
    | And
    | Or

type unop = Not    

type expr =
    | IntLit of int
    | BoolLit of bool
    | StrLit of string
    | Ident of string
    | Binop of expr * binop * expr
    | Unop of unop * expr
    | Assign of string * expr

type bind = typ * string

type stmt = 
    Expr of expr
  | Return of expr
  | Bind of bind
  | Seq of bind * expr
  | Blank

type func_decl = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  body: stmt list;
}

type program = bind list * func_decl list
