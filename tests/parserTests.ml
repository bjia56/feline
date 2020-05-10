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

		let () = assert ((TypIdent "STRIN", "x") =  List.nth ast.globals 0) in
		let () = assert ((Int, "y") = List.nth ast.globals 1) in
        ()
    ) with
    | Parsing.Parse_error -> failwith (Utils.syntax_error_string lex)

let test_parse_functions () =
    let raw = "
HAI ME TEH INTEGR FUNC test WIT x TEH INTEGR
    GIVEZ x
KTHXBAI

HAI ME TEH STRIN FUNC test2
    GIVEZ \"foo\"
KTHXBAI
" in
    let lex = Lexing.from_string raw in
    try (
        let ast = Parser.module_decl Scanner.token lex in
		let () = assert (0 = List.length ast.globals) in
        let () = assert (2 = List.length ast.functions) in
        let () = assert (0 = List.length ast.classes) in

        let f1 = List.nth ast.functions 0 in
        let () = assert ("test" = f1.fname) in
        let () = assert (Int = f1.rtyp) in
        let () = assert (1 = List.length f1.formals) in
        let () = assert ((Int, "x") = List.nth f1.formals 0) in
        let () = assert (1 = List.length f1.body) in
        let () = assert (Return (Ident "x") = List.nth f1.body 0) in

        let f2 = List.nth ast.functions 1 in
        let () = assert ("test2" = f2.fname) in
        let () = assert (TypIdent "STRIN" = f2.rtyp) in
        let () = assert (0 = List.length f2.formals) in
        let () = assert (1 = List.length f2.body) in
        let () = assert (Return (StrLit "foo") = List.nth f2.body 0) in
        ()
    ) with
    | Parsing.Parse_error -> failwith (Utils.syntax_error_string lex)

let test_parse_module () =
    let raw = "
PLS GIV STDIO? FIO?
PLS GIV ARG?

HAI ME TEH VARBL boo TEH INTEGR

HAI ME TEH CLAS foo
EVRYONE:
	DIS TEH CONS
	KTHXBAI

	DIS TEH VARBL baz TEH STRIN

	DIS TEH INTEGR FUNC hi
		I HAS A VARBL x TEH INTEGR
		x ITZ 4
		GIVEZ x
	KTHXBAI

	DIS TEH DES
	KTHXBAI
MESELF:
	DIS TEH VARBL bar TEH STRIN

	DIS TEH STRIN FUNC cap
		I HAS A VARBL y TEH STRIN ITZ bar
		GIVEZ y
	KTHXBAI
KTHXBAI

HAI ME TEH FUNC empty
KTHXBAI

HAI ME TEH INTEGR FUNC MAIN
    I HAS A VARBL unused TEH BUL
	4 PLUZ 4
	I HAS A VARBL obj TEH foo ITZ NU foo
	hi IN obj WIT KTHX =^..^= legal as a function call =^..^=
    baz IN obj =^..^= legal as variable =^..^=
	baz IN obj ITZ \"dog\" =^..^= legal variable reassignment (can’t reassign functions) =^..^=
	I HAS A VARBL b TEH STRIN ITZ baz IN obj =^..^= legal, variable baz is public in foo =^..^=
	I HAS A VARBL c TEH INTEGR ITZ hi IN obj WIT KTHX =^..^= legal, function hi is public in foo =^..^=
	b ITZ \"cat\" =^..^= legal =^..^=
	I HAS A VARBL a TEH STRIN ITZ cap IN obj WIT KTHX =^..^= illegal, func cap is private in foo =^..^=
	bar IN obj ITZ 4 =^..^= illegal, variable bar is private in foo =^..^=
	I HAS A VARBL f TEH INTEGR ITZ bar IN obj =^..^= illegal, variable bar is private in foo =^..^=
    GIVEZ 0
KTHXBAI
" in
    let lex = Lexing.from_string raw in
    try (
        let ast = Parser.module_decl Scanner.token lex in

        let () = assert (3 = List.length ast.imports) in
        let () = assert (1 = List.length ast.globals) in
        let () = assert (2 = List.length ast.functions) in
        let () = assert (1 = List.length ast.classes) in

        let () = assert (["STDIO"; "FIO"; "ARG"] = ast.imports) in
        let () = assert ([(Int, "boo")] = ast.globals) in

        let f1 = List.nth ast.functions 0 in
        let () = assert ("empty" = f1.fname) in
        let () = assert (Void = f1.rtyp) in
        let () = assert (0 = List.length f1.formals) in
        let () = assert (0 = List.length f1.body) in

        let f2 = List.nth ast.functions 1 in
        let () = assert ("MAIN" = f2.fname) in
        let () = assert (Int = f2.rtyp) in
        let () = assert (0 = List.length f2.formals) in
        let () = assert (13 = List.length f2.body) in
        let () = assert (
            [
                Bind (Bool, "unused");
                Expr (Binop ((IntLit 4), Add, (IntLit 4)));
                BindAssign ((TypIdent "foo", "obj"), NewInstance "foo");
                Expr (ClassFunctcall ("obj", ("hi", [])));
                Expr (ClassMemAccess ("baz", "obj"));
                ClassMemRassn ("baz", "obj", (StrLit "dog"));
                BindAssign ((TypIdent "STRIN", "b"), ClassMemAccess ("baz", "obj"));
                BindAssign ((Int, "c"), ClassFunctcall ("obj", ("hi", [])));
                Assign ("b", StrLit "cat");
                BindAssign ((TypIdent "STRIN", "a"), ClassFunctcall ("obj", ("cap", [])));
                ClassMemRassn ("bar", "obj", (IntLit 4));
                BindAssign ((Int, "f"), ClassMemAccess ("bar", "obj"));
                Return (IntLit 0);
            ] = f2.body
        ) in

        let c = List.nth ast.classes 0 in
        let () = assert ("foo" = c.cname) in
        let () = assert (1 = List.length c.pubmembers) in
        let () = assert (1 = List.length c.privmembers) in
        let () = assert (1 = List.length c.pubfuncs) in
        let () = assert (1 = List.length c.privfuncs) in
        let () = assert (1 = List.length c.cons) in
        let () = assert (1 = List.length c.des) in

        let () = assert ([(TypIdent "STRIN", "baz")] = c.pubmembers) in
        let () = assert ([(TypIdent "STRIN", "bar")] = c.privmembers) in
        let () = assert (
            [
                {
                    fname = "hi";
                    rtyp = Int;
                    formals = [];
                    body = [
                        Bind (Int, "x");
                        Assign ("x", (IntLit 4));
                        Return (Ident "x");
                    ];
                };
            ] = c.pubfuncs
        ) in
        let () = assert (
            [
                {
                    fname = "cap";
                    rtyp = TypIdent "STRIN";
                    formals = [];
                    body = [
                        BindAssign ((TypIdent "STRIN", "y"), (Ident "bar"));
                        Return (Ident "y");
                    ];
                };
            ] = c.privfuncs
        ) in
        let () = assert([ [] ] = c.cons) in
        let () = assert([ [] ] = c.des) in
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

    let () = print_endline "Running ParserTests.test_parse_functions" in
    let () = test_parse_functions () in
    let () = print_endline "Completed ParserTests.test_parse_functions" in

    let () = print_endline "Running ParserTests.test_parse_module" in
    let () = test_parse_module () in
    let () = print_endline "Completed ParserTests.test_parse_module" in

    ()
