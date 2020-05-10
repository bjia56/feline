(* Abstract Syntax Tree for Feline *)

type typ =
  | Null
  | Void
  | Int
  | Long
  | Bool
  (* | Array of typ * int  *)
  (* TODO: Implement Arrays *)
  | Exception of string
  | TypIdent of string
  (* Used internally *)
  | PtrAsInt

type binop = Add | Sub | Mul | Div | Neq | Eq | Less | Greater | And | Or

type unop = Not

type expr =
  | NullLit
  | IntLit of int
  | BoolLit of bool
  | StrLit of string
  | Ident of string
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | Functcall of functcall
  | NewInstance of string
  | ClassFunctcall of string * functcall
  | ClassMemAccess of string * string

and functcall = string * expr list

type bind = typ * string

type stmt =
  | Expr of expr
  | Return of expr
  | Bind of bind
  | BindAssign of bind * expr
  | Assign of string * expr
  | ClassMemRassn of string * string * expr
  | Dealloc of expr
  | If of expr * stmt list
  | IfElse of expr * stmt list * stmt list
  | While of expr * stmt list

type func_decl = {
  rtyp : typ;
  fname : string;
  formals : bind list;
  body : stmt list;
}

type cons_decl = stmt list

type des_decl = stmt list

type class_decl = {
  cname : string;
  pubmembers : bind list;
  privmembers : bind list;
  pubfuncs : func_decl list;
  privfuncs : func_decl list;
  cons : cons_decl list;
  (* always public *)
  des : des_decl list; (* always public *)
}

type module_decl = {
  imports : string list;
  classes : class_decl list;
  functions : func_decl list;
  globals : bind list;
}
