open Ast
open Utils

let rec eval (exp: expr) : fValue =
    match exp with
    | Ident(s) -> FelineString s

let _ =
    (*
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.expr Scanner.token lexbuf in
    let result = eval expr in
    *)
    let visible = Interpreter.find_function "VISIBLE" (Interpreter.find_module  "STDIO") in
    let read = Interpreter.find_function "READ" (Interpreter.find_module "STDIO") in
    visible [(read [])]

