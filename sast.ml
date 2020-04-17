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
  | SFunctcall of sfunctcall
and sfunctcall = string * sexpr list

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

type scons_decl = bind list * sstmt list

type sdes_decl = sstmt list

type sclass_decl = {
	scname: string;
  spubmembers: bind list;
  sprivmembers: bind list;
  spubfuncs: sfunc_decl list;
  sprivfuncs: sfunc_decl list;
  scons: scons_decl list;
  sdes: sdes_decl list;
}

type sprogram = {
    sclasses: sclass_decl list;
    sfunctions: sfunc_decl list;
    sglobals: bind list;
}


