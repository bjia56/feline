(* Abstract Syntax Tree for Feline *)

type typ =
    | Void
    | Null
    | Int
    | String
    | Bool
   (* | Array of typ * int  *) (* TODO: Implement Arrays *)
    | Exception of string
    | TypIdent of string

type binop =
    | Add
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
    | Call of string * expr list

type bind = typ * string

type stmt =
    | Expr of expr
    | Return of expr
    | Bind of bind
    | BindAssign of bind * expr
    | Assign of string * expr

type func_decl = {
    rtyp: typ;
    fname: string;
    formals: bind list;
    body: stmt list;
}

type class_decl = {
    cname: string;
    cfunctions: func_decl list;
    (* incomplete *)
}

type program = {
    classes: class_decl list;
    functions: func_decl list;
    globals: bind list;
}
