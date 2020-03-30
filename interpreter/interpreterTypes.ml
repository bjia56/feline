(*
 * Map types available to the interpreter
 *)

module FelineModMap = Map.Make(String)
module FelineFuncMap = Map.Make(String)
module FelineClassMap = Map.Make(String)
module FelineVarMap = Map.Make(String)

(*
 * Types available to the interpreter
 *)

type fType =
    | NullType
    | IntType
    | StringType
    | BoolType
    | CustomType of string

type fValue =
    | FelineNull
    | FelineInt of int
    | FelineString of string
    | FelineBool of bool
    | FelineCustom of string * fValue list

type fModule = (fValue list -> fValue) FelineFuncMap.t * string FelineClassMap.t

(*
 * Exceptions available to the interpreter
 *)

exception InternalInterpreterError
exception TypeMismatch
exception NotEnoughArguments
exception TooManyArguments
