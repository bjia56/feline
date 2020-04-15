open Ast

let test_parse_class () =
    let raw_class = "
HAI ME TEH CLAS foo

EVRYONE:
    DIS TEH CONS
    KTHXBAI

    DIS TEH VARBL baz TEH STRIN

    DIS TEH DES
    KTHXBAI

MESELF:
    DIS TEH VARBL bar TEH INTEGR

    DIS TEH STRIN FUNC cap
        GIVEZ \"test\"
    KTHXBAI
KTHXBAI
" in
    let lex = Lexing.from_string raw_class in
    try (
        let ast = Parser.program Scanner.token lex in
        let () = assert (0 = List.length ast.functions) in
        let () = assert (1 = List.length ast.classes) in

        let clas = List.hd ast.classes in
        let () = assert ("foo" = clas.cname) in
        let () = assert (1 = List.length clas.pubmembers) in
        let () = assert (1 = List.length clas.privmembers) in
        let () = assert (0 = List.length clas.pubfuncs) in
        let () = assert (1 = List.length clas.privfuncs) in
        let () = assert (1 = List.length clas.cons) in
        let () = assert (1 = List.length clas.des) in

        let pubmember_typ, pubmember_name = List.hd clas.pubmembers in
        let () = assert ("baz" = pubmember_name) in
        let () = assert (Ast.String = pubmember_typ) in
        ()
    ) with
    | Parsing.Parse_error -> failwith (ParserUtils.syntax_error_string lex)


let run_tests () =
    let () = print_endline "Running ParserTests..." in

    let () = print_endline "Running ParserTests.test_parse_class" in
    let () = test_parse_class () in
    let () = print_endline "Completed ParserTests.test_parse_class" in

    ()
