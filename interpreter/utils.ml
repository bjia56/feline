open InterpreterTypes

(*
 * Utility functions used by the interpreter
 *)
let int_of_fValue (v: fValue) : int =
    match v with
    | FelineInt(i) -> i
    | _ -> raise TypeMismatch

let bool_of_fValue (v: fValue) : bool =
    match v with
    | FelineBool(b) -> b
    | _ -> raise TypeMismatch

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
    | BoolType -> (
        match v with
        | FelineBool(b) -> true
        | _ -> false
    )
    | CustomType(s) -> (
        match v with
        | FelineCustom(n, f) -> s = n
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
