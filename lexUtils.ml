exception InvalidStringLiteral

let rec remove_escape_sequences x =
    if x = "" then "" else
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
