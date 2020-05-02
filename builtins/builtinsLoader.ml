module StringMap = Map.Make (String)

let builtins_list = [ STDIO.module_signature; GlobalBuiltins.module_signature ]

let builtin_modules =
  let visit_module m (name, ast) = StringMap.add name ast m in
  List.fold_left visit_module StringMap.empty builtins_list

let with_builtins (modules : Ast.module_decl StringMap.t) :
    Ast.module_decl StringMap.t =
  let dup_module name _ _ =
    raise (Failure ("builtin module " ^ name ^ " may not be redefined"))
  in
  StringMap.union dup_module builtin_modules modules

let library_src_dir =
  (* TODO fix this to be relative to the compiler *)
  "./builtins/src"
