let ast_of_chan chan =
    Parser.program Scanner.token (Lexing.from_channel chan)

let ast_of_file file_name =
    let file_chan = open_in file_name in
    let ast = ast_of_chan file_chan in
    let () = close_in file_chan in
    ast

let _ =
    let () = Printexc.record_backtrace true in
    try (
        (* Requires one cmd line argument, the input file *)
        let ast = ast_of_file Sys.argv.(1) in
        let res = Interpreter.eval_program ast in
        match res with
        | InterpreterTypes.FelineInt(i) -> print_string ((string_of_int i) ^ "\n")
        | _ -> ()
    ) with
    | _ -> Printexc.print_backtrace stdout
