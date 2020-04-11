{ open Parser }

rule token =

    parse [' ' '\t']                  { token lexbuf }
    | ['\n' '\r']+                         { NEWLINE }
    | '?'                                  { QUESTION }
    | "I"                                  { I }
    | "HAS"                                { HAS }
    | "A"                                  { A }
    | "VARBL"                              { VARBL }
    | "ITZ"                                { ITZ }
    | "GIVEZ"                              { GIVEZ }
    | "PLS"                                { PLS }
    | "GIV"                                { GIV }
    | "HAI"                                { HAI }
    | "ME"                                 { ME }
    | "TEH"                                { TEH }
    | "FUNC"                               { FUNC }
    | "MEOW"						       { MEOW }
    | "WIT"                                { WIT }
    | "KTHXBAI"                            { KTHXBAI }
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
    | ['0'-'9']+ as lit                    { INTEGR(int_of_string lit) }
    | "BUL"                                { BUL }
    | "YEZ"                                { BLIT(true)  }
    | "NO"                                 { BLIT(false) }
    | "\"([^\"\\\\]|\\\\.)*\"" as str      { STRIN (str) }
    | ['a'-'z' 'A'-'Z' '_']
      ['a'-'z' 'A'-'Z' '_' '0'-'9']* as id { IDENT(id) }
    (*| '"'							       {QUOTATION}    *)
    | eof                                  { EOF }
