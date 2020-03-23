type typ = Int | String | Ident of string

type expr =
    | Ident of string

type func_decl = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  body: stmt list;
}

type bind = typ * string

type stmt = 
    Expr of expr
  | Return of expr
  | Bind of bind
  | Seq of bind * expr
