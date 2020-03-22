type typ = Int | String | Ident of string

type expr =
    | Ident of string

type bind = typ * string

type func_decl = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals bind list:
  body: stmt list;
}

type stmt = 
    Expr of expr
  | Return of expr
