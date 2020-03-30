open InterpreterTypes
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

let exported_funcs = FelineFuncMap.empty
let exported_funcs = FelineFuncMap.add "MEOW" meow exported_funcs
let exported_funcs = FelineFuncMap.add "NOM" nom exported_funcs

let exported_classes = FelineClassMap.empty

let exported_module = (exported_funcs, exported_classes)
