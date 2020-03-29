{ open Parser }

rule token =
    parse [' ' '\t' '\r' '\n']             { token lexbuf }
    | '?'                                    { QUESTION }
    | "PLS"                                { PLS }
    | "GIV"                                { GIV }
    | "HAI"                                { HAI }
    | "ME"                                 { ME }
    | "TEH"                                { TEH }
    | "FUNC"                               { FUNC }
    | “MEOW”						       {MEOW}
    | "WIT"                                { WIT }
    | "KTHXBAI"                            { KTHXBAI }
    | ['0'-'9']+ as lit                    { INTLIT(int_of_string lit) }
    | ['a'-'z' 'A'-'Z' '_']
      ['a'-'z' 'A'-'Z' '_' '0'-'9']* as id { IDENT(id) }

    |’ ” ’							  {QUOTATION}    
    |  eof        	  	                      { EOF }

