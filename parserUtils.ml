open Lexing

exception SyntaxError of string

let syntax_error_string lexbuf =
    let pos = lexbuf.lex_curr_p in
    if pos.pos_fname = "" then
        "Syntax error on line " ^ (string_of_int pos.pos_lnum) ^ " column " ^ (string_of_int pos.pos_cnum)
    else
        "Syntax error in file " ^ pos.pos_fname ^ " on line " ^ (string_of_int pos.pos_lnum) ^ " column " ^ (string_of_int pos.pos_cnum)

let concat_lists x y = List.rev_append (List.rev x) y
