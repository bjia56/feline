open TestUtils

let test_parse_class () =
    let raw_class = "
HAI ME TEH CLAS foo
EVRYONE :
    DIS TEH CONS
    KTHXBAI

    DIS TEH DES
    KTHXBAI
KTHXBAI
" in
    let ast = Parser.program Scanner.token (Lexing.from_string raw_class) in
    let _ = ast in
    ()


let run_tests () =
    let () = print_endline "Running ParserTests" in

    let () = print_tabbed "Running test_parse_class" in
    let () = test_parse_class () in
    let () = print_tabbed "Completed test_parse_class" in

    ()
