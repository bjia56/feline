open Sast
module StringMap = Map.Make (String)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let module_decl = Parser.module_decl Scanner.token lexbuf in
  let asts = StringMap.add "cmdline" module_decl StringMap.empty in
  Semant.check_module (BuiltinsLoader.with_builtins asts) StringMap.empty module_decl
  (* let sprogram = *)
  (* Semant.check_module (program.classes, program.functions, program.globals) *)

(*in print_endline (string_of_sprogram sprogram) *)
