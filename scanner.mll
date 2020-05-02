{
open Parser
open Utils
}

rule token =
    parse [' ' '\t' '\r']                  { token lexbuf }
    | ['\n']                               { Lexing.new_line lexbuf; NEWLINE }
    | "I"                                  { I }
    | "HAS"                                { HAS }
    | "A"                                  { A }
    | "VARBL"                              { VARBL }
    | "ITZ"                                { ITZ }
    | "NU"                                 { NU }
    | "DELET"                              { DELET }
    | "GIVEZ"                              { GIVEZ }
    | "PLS"                                { PLS }
    | "GIV"                                { GIV }
    | "HAI"                                { HAI }
    | "ME"                                 { ME }
    | "TEH"                                { TEH }
    | "FUNC"                               { FUNC }
    | "CLAS"                               { CLAS }
    | "CONS"                               { CONS }
    | "DIS"                                { DIS }
    | "DES"                                { DES }
    | "WIT"                                { WIT }
    | "IN"                                 { IN }
    | "KTHXBAI"                            { KTHXBAI }
    | "EVRYONE:"                           { EVRYONE }
    | "MESELF:"                            { MESELF }
    | "KTHX"                               { KTHX }
    | "PLUZ"                               { PLUZ }
    | "MYNUZ"                              { MYNUZ }
    | "TYMEZ"                              { TYMEZ }
    | "DIVYD"                              { DIVYD }
    | "NOT" [' ' '\t']+ "SAYM"
      [' ' '\t']+ "AZ"                     { NOT_SAYM_AZ }
    | "SAYM" [' ' '\t']+ "AZ"              { SAYM_AZ }
    | "BIGGR" [' ' '\t']+ "THAN"           { BIGGR_THAN }
    | "SMALLR" [' ' '\t']+ "THAN"          { SMALLR_THAN }
    | "AN"                                 { AN }
    | "OR"                                 { OR }
    | "OPOZIT"                             { OPOZIT }
    | ['0'-'9']+ as lit                    { INTLIT(int_of_string lit) }
    | "YEZ"                                { BLIT(true)  }
    | "NO"                                 { BLIT(false) }
    | '"' (
        [^'"'] | "\\\"" |
        "\\\\" | "\\n" |
        "\\t"
      )* '"' as str                        { STRLIT(sanitize_str_lit str) }
    | "INTEGR"                             { INTEGR }
    | "BUL"                                { BUL }
    | "NOL"                                { NOL }
    | ['a'-'z' 'A'-'Z' '_']
      ['a'-'z' 'A'-'Z' '_' '0'-'9']* as id { IDENT(id) }
    | ['a'-'z' 'A'-'Z' '_']
      ['a'-'z' 'A'-'Z' '_' '0'-'9']*
      '?' as id                            { IDENT_QUESTION(List.hd (String.split_on_char '?' id )) }
    | eof                                  { EOF }
