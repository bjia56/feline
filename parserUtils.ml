open Lexing

exception SyntaxError of string

let current_file = ref ""

let update_current_file s =
  current_file := s

let syntax_error_string lexbuf =
  let pos = lexbuf.lex_curr_p in
  let line = string_of_int pos.pos_lnum in
  let col = string_of_int (pos.pos_cnum - pos.pos_bol) in
  if !current_file = "" then
    "Syntax error on line " ^ line ^ " column " ^ col
  else
    "Syntax error in file " ^ !current_file ^ " on line "
    ^ line ^ " column " ^ col

let concat_lists x y = List.rev_append (List.rev x) y
