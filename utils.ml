open Lexing
open Ast

(* Pretty printing functions *)
let string_of_typ = function
  | Void -> "void"
  | Null -> "null"
  | Int -> "int"
  | Bool -> "bool"
  | Exception s -> "exception " ^ s
  | TypIdent s -> "custom " ^ s
  | PtrAsInt -> "pointer"

exception SyntaxError of string
exception InvalidStringLiteral

let rec remove_escape_sequences x =
  if x = "" then ""
  else
    let len = String.length x in
    match x.[0] with
    | '\\' ->
        if x.[1] = '\\' then
          "\\" ^ remove_escape_sequences (String.sub x 2 (len - 2))
        else if x.[1] = '"' then
          "\"" ^ remove_escape_sequences (String.sub x 2 (len - 2))
        else if x.[1] = 'n' then
          "\n" ^ remove_escape_sequences (String.sub x 2 (len - 2))
        else if x.[1] = 't' then
          "\t" ^ remove_escape_sequences (String.sub x 2 (len - 2))
        else raise InvalidStringLiteral
    | _ ->
        Char.escaped x.[0] ^ remove_escape_sequences (String.sub x 1 (len - 1))

let sanitize_str_lit x =
  let len = String.length x in
  let noquotes = String.sub x 1 (len - 2) in
  let unescaped = remove_escape_sequences noquotes in
  unescaped

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
