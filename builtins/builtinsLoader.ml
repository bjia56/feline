module StringMap = Map.Make (String)

let builtins_list = [ STDIO.module_signature ]

let builtin_modules =
  let parser_builder m (name, contents) =
    let lex = Lexing.from_string contents in
    let ast = Parser.module_decl Scanner.token lex in
    StringMap.add name ast m
  in
  List.fold_left parser_builder StringMap.empty builtins_list

let with_builtins (modules : Ast.module_decl StringMap.t) :
    Ast.module_decl StringMap.t =
  let dup_module name _ _ =
    raise (Failure ("builtin module " ^ name ^ " may not be redefined"))
  in
  StringMap.union dup_module builtin_modules modules

let library_src_dir =
  (* TODO fix this to be relative to the compiler *)
  "./builtins/src"
