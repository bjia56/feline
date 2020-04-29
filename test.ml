open Sast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  (* let sprogram = *)
  Semant.check (program.classes, program.functions, program.globals)

(*in print_endline (string_of_sprogram sprogram) *)
