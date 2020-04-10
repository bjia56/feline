let ast_of_chan chan =
    Parser.program Scanner.token (Lexing.from_channel chan)

let ast_of_file file_name =
    let file_chan = open_in file_name in
    let ast = ast_of_chan file_chan in
    let () = close_in file_chan in
    ast

let empty_string_list: string list = []

module AstMap = Map.Make(String)
exception InvalidFelineFileName of string

let rec split_string (c: char) (s: string): string list =
    if s = "" then
        []
    else
        let len = String.length s in
        if c = s.[0] then
            ""::(split_string c (String.sub s 1 (len - 1)))
        else
            let tail_list = split_string c (String.sub s 1 (len - 1)) in
            match tail_list with
            | [] -> [Char.escaped s.[0]]
            | hd::tl -> (Char.escaped s.[0] ^ hd)::tl

let validate_and_trim_file_extension (file: string): string =
    let components = split_string '.' file in
    if 2 <> List.length components then
        raise (InvalidFelineFileName file)
    else
        let first = List.nth components 0 in
        let second = List.nth components 1 in
        if second <> "cat" then
            raise (InvalidFelineFileName file)
        else
            first

let get_module_name (file: string): string =
    let trimmed = validate_and_trim_file_extension file in
    let components = split_string '/' trimmed in
    List.hd (List.rev components)

let rec asts_of_file_list (files: string list): Ast.program AstMap.t =
    match files with
    | [] -> AstMap.empty
    | hd::tl ->
        let tl_result = asts_of_file_list tl in
        let module_name = get_module_name hd in
        AstMap.add module_name (ast_of_file hd) tl_result

let print_parsed_modules (asts: Ast.program AstMap.t) =
    let is_first = ref true in
    let print_binding k v =
        if not !is_first then
            let () = print_string ", " in
            print_string k
        else
            let () = is_first := false in
            print_string k
    in
    let () = print_string "Found modules: " in
    let () = AstMap.iter print_binding asts in
    print_endline ""

let _ =
    let files = ref empty_string_list in
    let testcases = ref false in
    let speclist = [
        ("-file", Arg.String (
            fun x ->
                let () = files := List.rev (x::(List.rev !files)) in
                ()
        ), "Input file to compile");
        ("-test", Arg.Set testcases, "Run compiler test cases");
    ] in
    let usage = "Compiler for the FELINE programming language. Options available:" in
    let () = Arg.parse speclist (fun x -> ()) usage in

    if !testcases then
        (* run tests *)
        ()
    else
        let () = Printexc.record_backtrace true in
        let () = print_endline ("Compiling " ^ (string_of_int (List.length !files)) ^ " files...") in
        try (
            (* Requires one cmd line argument, the input file *)
            let asts = asts_of_file_list !files in
            print_parsed_modules asts
        ) with
        | _ -> Printexc.print_backtrace stdout
