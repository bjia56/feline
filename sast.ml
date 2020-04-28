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
    | SClassFunctcall of string * sfunctcall
    | SClassMemAccess of string * string * int (* member, instance, member index *)
and sfunctcall = string * sexpr list

 type sstmt =
    | SExpr of sexpr
    | SReturn of sexpr
    | SBind of bind
    | SBindAssign of bind * sexpr
    | SAssign of string * sexpr
    | SClassMemRassn of string * string * sexpr
    | SInstance of bind

type sfunc_decl = {
    srtyp: typ;
    sfname: string;
    sformals: bind list;
    sbody: sstmt list;
}

type scons_decl = sstmt list

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
