(*
 * Types available to the interpreter
 *)

type fType =
    | NullType
    | IntType
    | StringType

type fValue =
    | FelineNull
    | FelineInt of int
    | FelineString of string

(*
 * Map types available to the interpreter
 *)

module FelineModMap = Map.Make(String)
module FelineFuncMap = Map.Make(String)

(*
 * Exceptions available to the interpreter
 *)

exception InternalInterpreterError
exception TypeMismatch
exception NotEnoughArguments
exception TooManyArguments

(*
 * Utility functions used by the interpreter
 *)
let check_var_type (v: fValue) (t: fType) : bool =
    match t with
    | NullType -> (
        match v with
        | FelineNull -> true
        | _ -> false
    )
    | IntType -> (
        match v with
        | FelineInt(i) -> true
        | _ -> false
    )
    | StringType -> (
        match v with
        | FelineString(s) -> true
        | _ -> false
    )

let rec check_args (values: fValue list) (types: fType list) =
    match values with
    | [] ->
        if 0 = List.length types then
            ()
        else
            raise NotEnoughArguments
    | head::tail ->
        if 0 = List.length types then
            raise TooManyArguments
        else
            if check_var_type head (List.hd types) then
                check_args tail (List.tl types)
            else
                raise TypeMismatch
