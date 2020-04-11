let test_parse_class () =
    let raw_class = "
HAI ME TEH CLAS foo
EVRYONE:
    DIS TEH CONS
    KTHXBAI

    DIS TEH DES
    KTHXBAI
KTHXBAI
" in
    let lex = Lexing.from_string raw_class in
    try (
        let _ = Parser.program Scanner.token lex in
        ()
    ) with
    | Parsing.Parse_error -> print_endline (ParserUtils.syntax_error_string lex)


let run_tests () =
    let () = print_endline "Running ParserTests" in

    let () = print_endline "Running test_parse_class" in
    let () = test_parse_class () in
    let () = print_endline "Completed test_parse_class" in

    ()
