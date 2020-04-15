(* Semantically-check Abstract Syntax Tree for Feline *)

open Ast

type sexpr = type * sx
and sx = 
	SIntLit of int
  | SBoolLit of bool
  | SStrLit of string
  | SIdent of string
  | SBinop of sexpr * binop * sexpr
  | SUnop of unop * sexpr
  | SCall of string * sexpr list

 type sstmt = 
  | SExpr of expr
  | SReturn of expr
  | SBind of bind
  | SBindAssign of bind * expr
  | SAssign of string * expr

type sfunc_decl = {
    srtyp: typ;
    sfname: string;
    sformals: bind list;
    sbody: stmt list;
}

type sclass_decl = {
	scname: string;
	scfunctions: func_decl list;
}

type program = {
    sclasses: class_decl list;
    sfunctions: func_decl list;
    sglobals: bind list;
}


