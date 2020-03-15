let ast_of_chan chan =
    Parser.expr Scanner.token (Lexing.from_channel chan)

let ast_of_file file_name =
    let file_chan = open_in file_name in
    let ast = ast_of_chan file_chan in
    let () = close_in file_chan in
    ast

let _ =
    (*
    if Array.length Sys.argv < 2 then
        let ast = ast_of_chan stdin in
        Interpreter.eval ast
    else
        let ast = ast_of_file Sys.argv.(1) in
        Interpreter.eval ast
    *)
    let stdio_m = Interpreter.find_module "STDIO" in
    let meow = Interpreter.find_function "MEOW" stdio_m in
    let nom = Interpreter.find_function "NOM" stdio_m in
    meow [(nom [])]

