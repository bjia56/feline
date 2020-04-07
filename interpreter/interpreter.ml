(*
 * This file includes runtime state and items used by the
 * interpreter.
 *)

open InterpreterTypes
open Utils
open Ast

let builtin_modules = FelineModMap.empty
let builtin_modules = FelineModMap.add "STDIO" STDIO.exported_module builtin_modules

type global_context = {
    functions: (fValue list -> fValue) FelineFuncMap.t;
    classes: (fValue list -> fValue) FelineClassMap.t;
}

type local_context = {
    variables: fValue FelineVarMap.t;
}

let rec eval_expr (g: global_context ref) (c: local_context ref) (e: expr) : fValue =
    match e with
    | IntLit(i) -> FelineInt(i)
    | BoolLit(b) -> FelineBool(b)
    | StrLit(s) -> FelineString(s)
    | Ident(s) -> FelineVarMap.find s !c.variables
    | Binop(e1, b, e2) -> (
        let e1_v = eval_expr g c e1 in
        let e2_v = eval_expr g c e2 in
        match b with
        | Add ->
            let res = (int_of_fValue e1_v) + (int_of_fValue e2_v) in
            FelineInt(res)
        | Sub ->
            let res = (int_of_fValue e1_v) - (int_of_fValue e2_v) in
            FelineInt(res)
        | Mul ->
            let res = (int_of_fValue e1_v) * (int_of_fValue e2_v) in
            FelineInt(res)
        | Div ->
            let res = (int_of_fValue e1_v) / (int_of_fValue e2_v) in
            FelineInt(res)
        | Neq -> (* need to add support for other type comparisons *)
            let res = (int_of_fValue e1_v) <> (int_of_fValue e2_v) in
            FelineBool(res)
        | Eq -> (* need to add support for other type comparisons *)
            let res = (int_of_fValue e1_v) = (int_of_fValue e2_v) in
            FelineBool(res)
        | Less ->
            let res = (int_of_fValue e1_v) < (int_of_fValue e2_v) in
            FelineBool(res)
        | Greater ->
            let res = (int_of_fValue e1_v) > (int_of_fValue e2_v) in
            FelineBool(res)
        | And ->
            let res = (bool_of_fValue e1_v) && (bool_of_fValue e2_v) in
            FelineBool(res)
        | Or ->
            let res = (bool_of_fValue e1_v) || (bool_of_fValue e2_v) in
            FelineBool(res)
    )
    | Unop(u, e) -> ( (* should this operator be overloaded to support negating integers? *)
        let e_val = eval_expr g c e in
        match u with
        | Not ->
            let res = not (bool_of_fValue e_val) in
            FelineBool(res)
    )
    | Functcall(f) ->
        let fname, fargs = f in
        let farg_vals = List.map (eval_expr g c) fargs in
        let func = FelineFuncMap.find fname !g.functions in
        func farg_vals

let eval_stmt (g: global_context ref) (c: local_context ref) (s: stmt) : fValue option =
    match s with
    | Expr(e) -> let _ = eval_expr g c e in None
    | Return(e) -> Some(eval_expr g c e)
    | Bind(b) ->
        let t, name = b in
        let defaultVal = (
            match t with
            | Null -> FelineNull
            | Int -> FelineInt(0)
            | String -> FelineString("")
            | TypIdent(s) -> FelineNull (* TODO implement custom types *)
        ) in
        let _ = (c := { variables=FelineVarMap.add name defaultVal !c.variables }) in
        None
    | BindAssign(b, e) ->
        let t, name = b in
        let exp = eval_expr g c e in (* TODO type check *)
        let _ = (c := { variables=FelineVarMap.add name exp !c.variables }) in
        None
    | Assign(s, e) -> (* TODO raise error if var is not defined *)
        let exp = eval_expr g c e in
        let _ = (c := { variables=FelineVarMap.add s exp !c.variables }) in
        None

let rec eval_func_stmts (g: global_context ref) (c: local_context ref) (sl: stmt list) : fValue =
    match sl with
    | [] -> FelineNull
    | hd::tl -> (
        let curr_stmt_res = eval_stmt g c hd in
        match curr_stmt_res with
        | None -> eval_func_stmts g c tl
        | Some(v) -> v
    )

let build_function (g: global_context ref) (f: func_decl) : (fValue list -> fValue) =
    let expected_args = List.map (
        fun formal ->
            let t, _ = formal in
            match t with
            | Null -> NullType
            | Int -> IntType
            | String -> StringType
            | TypIdent(s) -> CustomType(s)
    ) f.formals in

    let func_context = ref {
        variables=FelineVarMap.empty;
    } in

    (
        fun (args: fValue list) ->
            let () = check_args args expected_args in
            let _ = List.map2 (
                fun formal arg ->
                    let _, name = formal in
                    func_context := { variables=FelineVarMap.add name arg !func_context.variables }
            ) f.formals args in

            eval_func_stmts g func_context f.body
    )

let rec eval_program (program: program) : fValue =
    (* construct execution context *)
    let gcontext = ref {
        functions=STDIO.exported_funcs;
        classes=FelineClassMap.empty;
    } in

    (* extract function names, in order *)
    let fnames = List.map (fun (x: func_decl) -> x.fname) program.functions in
    (* build interpretable functions, in order *)
    let interpFuncs = List.map (build_function gcontext) program.functions in
    (* add functions to context *)
    let _ = List.map2 (
        fun fname ifunc ->
            gcontext := {
                functions=FelineFuncMap.add fname ifunc !gcontext.functions;
                classes=(!gcontext.classes);
            }
    ) fnames interpFuncs in

    (* run MAIN *)
    let main = FelineFuncMap.find "MAIN" !gcontext.functions in
    main []
