open Utils

let meow (args: fValue list) : fValue =
    let () = check_args args [StringType] in
    match List.hd args with
    | FelineString(s) ->
        let () = print_endline s in
        FelineNull
    | _ -> raise InternalInterpreterError

let nom (args: fValue list) : fValue =
    let () = check_args args [] in
    let s = read_line () in
    FelineString(s)

let exported = FelineFuncMap.empty
let exported = FelineFuncMap.add "MEOW" meow exported
let exported = FelineFuncMap.add "NOM" nom exported
