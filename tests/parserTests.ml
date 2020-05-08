open Ast

let test_parse_class () =
    let raw = "
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
    let lex = Lexing.from_string raw in
    try (
        let ast = Parser.module_decl Scanner.token lex in
        let () = assert (0 = List.length ast.globals) in
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
        let () = assert (Ast.TypIdent "STRIN" = pubmember_typ) in
        ()
    ) with
    | Parsing.Parse_error -> failwith (Utils.syntax_error_string lex)

let test_parse_globals () =
    let raw = "
HAI ME TEH VARBL x TEH STRIN
HAI ME TEH VARBL y TEH INTEGR
" in
    let lex = Lexing.from_string raw in
    try (
        let ast = Parser.module_decl Scanner.token lex in
		let () = assert (2 = List.length ast.globals) in
        let () = assert (0 = List.length ast.functions) in
        let () = assert (0 = List.length ast.classes) in

		let first_typ, first_name = List.nth ast.globals 0 in
		let second_typ, second_name = List.nth ast.globals 1 in

        let () = assert (first_typ = TypIdent "STRIN") in
        let () = assert (first_name = "x") in
        let () = assert (second_typ = Int) in
        let () = assert (second_name = "y") in
        ()
    ) with
    | Parsing.Parse_error -> failwith (Utils.syntax_error_string lex)


let run_tests () =
    let () = print_endline "Running ParserTests..." in

    let () = print_endline "Running ParserTests.test_parse_class" in
    let () = test_parse_class () in
    let () = print_endline "Completed ParserTests.test_parse_class" in

    let () = print_endline "Running ParserTests.test_parse_globals" in
    let () = test_parse_globals () in
    let () = print_endline "Completed ParserTests.test_parse_globals" in

    ()
