open Ast

let rec eval exp =
    match exp with
    | Ident(s) -> s

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.expr Scanner.token lexbuf in
    let result = eval expr in
    print_endline result
