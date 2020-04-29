open Ast

(* Pretty printing functions *)
let string_of_typ = function
  | Void -> "void"
  | Null -> "null"
  | Int -> "int"
  | String -> "string"
  | Bool -> "bool"
  | Exception s -> "exception " ^ s
  | TypIdent s -> "custom " ^ s
