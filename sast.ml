(* Semantically-check Abstract Syntax Tree for Feline *)

open Ast

type sexpr = typ * sx

and sx =
  | SNullLit
  | SIntLit of int
  | SBoolLit of bool
  | SStrLit of string
  | SIdent of string
  | SBinop of sexpr * binop * sexpr
  | SUnop of unop * sexpr
  | SFunctcall of sfunctcall
  | SNewInstance of string
  | SClassFunctcall of string * sfunctcall
  (* member, instance, member index *)
  | SClassMemAccess of string * string * int

and sfunctcall = string * sexpr list

type sstmt =
  | SExpr of sexpr
  | SReturn of sexpr
  | SBind of bind
  | SBindAssign of bind * sexpr
  | SAssign of bind * sexpr
  (* member, instance, member index, sexpr *)
  | SClassMemRassn of bind * string * int * sexpr
  | SDealloc of sexpr
  | SIf of sexpr * sstmt list
  | SIfElse of sexpr * sstmt list * sstmt list
  | SWhile of sexpr * sstmt list

type sfunc_decl = {
  srtyp : typ;
  sfname : string;
  sformals : bind list;
  sbody : sstmt list;
}

type scons_decl = sstmt list

type sdes_decl = sstmt list

type sclass_decl = {
  scname : string;
  spubmembers : bind list;
  sprivmembers : bind list;
  spubfuncs : sfunc_decl list;
  sprivfuncs : sfunc_decl list;
  scons : scons_decl;
  sdes : sdes_decl;
}

type smodule = {
  sclasses : sclass_decl list;
  sfunctions : sfunc_decl list;
  sglobals : bind list;
  sclass_imports : sclass_decl list;
  sfunction_imports : sfunc_decl list;
  sglobal_imports : bind list;
}
