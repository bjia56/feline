open Utils

let visible (args: fValue list) : fValue =
    let arg_vars = values_to_vars args [StringType] in
    let () = check_args_or_fail arg_vars in
    let (_, v) = List.hd arg_vars in
    match v with
    | FelineString(s) ->
        let () = print_endline s in
        FelineNull
    | _ -> raise InternalInterpreterError

let read (args: fValue list) : fValue =
    let arg_vars = values_to_vars args [] in
    let () = check_args_or_fail arg_vars in
    let s = read_line () in
    FelineString(s)

let exported = FelineFuncMap.empty
let exported = FelineFuncMap.add "VISIBLE" visible exported
let exported = FelineFuncMap.add "READ" read exported
