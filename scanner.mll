{ open Parser }

rule token =
    parse [' ' '\t' '\r']             { token lexbuf }
    | '\n'				   { NEWLINE  }
    | '?'                                  { QUESTION }
    | "I"				   { I }
    | "HAS"		                   { HAS }
    | "A"				   { A }
    | "VARBL"				   { VARBL }
    | "ITZ"				   { ITZ }
    | "GIVEZ"				   { GIVEZ }	
    | "PLS"                                { PLS }
    | "GIV"                                { GIV }
    | "HAI"                                { HAI }
    | "ME"                                 { ME }
    | "TEH"                                { TEH }
    | "FUNC"                               { FUNC }
    | "WIT"                                { WIT }
    | "KTHXBAI"                            { KTHXBAI }
    | "PLUZ"				   { PLUS    }
    | "MYNUZ"				   { MINUS   }
    | "TYMEZ"				   { TIMES   }
    | "DIVYD"			           { DIVIDE  }
    | "NOT"				   { NOT     }
    | "SAYM"				   { SAYM    }
    | "AZ"				   { AZ      }
    | "BIGGR"				   { BIGGR   }
    | "SMALLR" 				   { SMALLR  }
    | "THAN"				   { THAN    }
    | "AN"				   { AND     }
    | "OR"				   { OR      }
    | "OPOZIT"				   { NOT     } 
    | ['0'-'9']+ as lit                    { INTEGR(int_of_string lit) }
    | "YEZ"				   { BLIT(true)  }
    | "NO"				   { BLIT(false) }
    | "\"([^\"\\\\]|\\\\.)*\"" as str      { STRIN (str) }
    | ['a'-'z' 'A'-'Z' '_']
      ['a'-'z' 'A'-'Z' '_' '0'-'9']* as id { IDENT(id) }
    | eof				   { EOF       }
