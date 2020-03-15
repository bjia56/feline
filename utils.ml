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

type fVariable = fType * fValue

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
 * Utility functions
 *)
let check_var_type (arg: fVariable) : bool =
    let (arg_t, arg_v) = arg in
    match arg_t with
    | NullType -> (
        match arg_v with
        | FelineNull -> true
        | _ -> false
    )
    | IntType -> (
        match arg_v with
        | FelineInt(i) -> true
        | _ -> false
    )
    | StringType -> (
        match arg_v with
        | FelineString(s) -> true
        | _ -> false
    )

let rec check_args (args: fVariable list) : bool =
    match args with
    | [] -> true
    | hd::tail ->
        if check_var_type hd then
            check_args tail
        else
            false

let check_args_or_fail (args: fVariable list) =
    if not (check_args args) then
        raise TypeMismatch

let rec values_to_vars (values: fValue list) (var_types: fType list) : (fVariable list) =
    match values with
    | [] ->
        if 0 = List.length var_types then
            []
        else
            raise NotEnoughArguments
    | hd :: tail ->
        if 0 = List.length var_types then
            raise TooManyArguments
        else
            ((List.hd var_types), hd) :: (values_to_vars tail (List.tl var_types))
