(*
 * This file includes runtime state and items used by the
 * interpreter.
 *)

open Utils

let loaded_modules = FelineModMap.empty
let loaded_modules = FelineModMap.add "STDIO" STDIO.exported loaded_modules

let find_module (m: string) =
    FelineModMap.find m loaded_modules

let find_function (f: string) m =
    FelineFuncMap.find f m
