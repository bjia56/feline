open Sast
open Ast

module L = Llvm

module StringMap = Map.Make(String)

exception Unimplemented of string

let main_fdecl = {
    srtyp=Ast.Int;
    sfname="main";
    sformals=[];
    sbody=[SReturn(Ast.Int, SFunctcall("MAIN", []))]
}

let translate (mod_name: string) (p: sprogram) =
    let context = L.global_context () in
    let the_module = L.create_module context mod_name in
    let class_lltype_map : (L.lltype StringMap.t) ref = ref StringMap.empty in

    (* Get types from the context *)
    let i32_t = L.i32_type context
    and i8_t = L.i8_type context
    and i1_t = L.i1_type context
    and void_t = L.void_type context
    in

    (* Helper to convert A type to L type *)
    let rec ltype_of_typ (t: Ast.typ) =
        match t with
        | Ast.Int -> i32_t
        | Ast.Bool -> i1_t
        | Ast.Void -> void_t
        | Ast.TypIdent s -> StringMap.find s !class_lltype_map
        | Ast.Pointer t -> L.pointer_type (ltype_of_typ t)
        | _ -> raise (Unimplemented ("cannot convert unimplemented type " ^ (AstUtils.string_of_typ t)))
    in

    let init_of_typ (t: Ast.typ) =
        match t with
        | Ast.Int -> L.const_int (ltype_of_typ t) 0
        | Ast.Bool -> L.const_int (ltype_of_typ t) 0
        | _ -> raise (Unimplemented "cannot init unimplemented type")
    in

    (* Construct global funcs *)
    let has_main = ref false in
    let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
        let function_decl m fdecl =
            let m2 = (
                if fdecl.sfname = "MAIN" then
                    let () = has_main := true in
                    (* Construct wrapper around "MAIN" *)
                    let ftype = L.function_type (ltype_of_typ Ast.Int) (Array.of_list []) in
                    StringMap.add main_fdecl.sfname (L.define_function main_fdecl.sfname ftype the_module, main_fdecl) m
                else m
            ) in

            let name = fdecl.sfname
            and formal_types =
                Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
            in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
            StringMap.add name (L.define_function name ftype the_module, fdecl) m2 in
        List.fold_left function_decl StringMap.empty p.sfunctions in

    (* Construct classes *)
    let class_decls : (L.lltype * (L.llvalue * sfunc_decl) StringMap.t * sclass_decl) StringMap.t =
        let class_decl m cdecl =
            (* Construct class type first *)
            let name = cdecl.scname in
            let members = List.map (fun (t,_) -> ltype_of_typ t) (List.rev_append (List.rev cdecl.spubmembers) cdecl.sprivmembers) in
            let ctype = L.named_struct_type context name in
            let () = L.struct_set_body ctype (Array.of_list members) false in

            let () = class_lltype_map := StringMap.add name ctype !class_lltype_map in

            (* Construct class functions *)
            let class_func_decl m fdecl =
                let name = fdecl.sfname
                and formal_types =
                    Array.of_list ((L.pointer_type ctype)::List.tl (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals))
                in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
                StringMap.add name (L.define_function name ftype the_module, fdecl) m in

            let class_cons_decl m consdecl =
                class_func_decl m {
                    srtyp=Ast.Void;
                    sfname=name ^ "_CONS";
                    sformals=[Pointer (TypIdent name), "DIS"];
                    sbody=consdecl
                }
            in
            let class_des_decl m desdecl =
                class_func_decl m {
                    srtyp=Ast.Void;
                    sfname=name ^ "_DES";
                    sformals=[Pointer (TypIdent name), "DIS"];
                    sbody=desdecl
                }
            in

            let cfuncs = List.fold_left class_func_decl StringMap.empty cdecl.spubfuncs in
            let cfuncs = List.fold_left class_func_decl cfuncs cdecl.sprivfuncs in
            let cfuncs = List.fold_left class_cons_decl cfuncs cdecl.scons in
            let cfuncs = List.fold_left class_des_decl cfuncs cdecl.sdes in

            StringMap.add name (ctype, cfuncs, cdecl) m in
        List.fold_left class_decl StringMap.empty p.sclasses in

    (* Add class functions to global functions *)
    let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
        let funcs = ref function_decls in
        let class_handler cname (_, m, _) =
            let class_func_handler fname v =
                funcs := StringMap.add fname v !funcs in
            StringMap.iter class_func_handler m in
        StringMap.iter class_handler class_decls;
        !funcs in
    let all_functions =
        let funcs = ref p.sfunctions in
        let class_handler cname (_, m, _) =
            let class_func_handler fname (_, fdecl) =
                funcs := fdecl::!funcs in
            StringMap.iter class_func_handler m in
        StringMap.iter class_handler class_decls;
        !funcs in

    (* Construct global vars *)
    let global_vars : L.llvalue StringMap.t =
        let global_var m (t, n) =
            let init = init_of_typ t
            in StringMap.add n (L.define_global n init the_module) m in
        List.fold_left global_var StringMap.empty p.sglobals in

    (* Define printf *)
    let printf_t : L.lltype =
        L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func : L.llvalue =
        L.declare_function "printf" printf_t the_module in

    (* Construct function body statements *)
    let build_function_body fdecl =
        let (the_function, _) = StringMap.find fdecl.sfname function_decls in
        let builder = L.builder_at_end context (L.entry_block the_function) in

        let str_format_str = L.build_global_stringptr "%s\n" "sfmt" builder in
        let int_format_str = L.build_global_stringptr "%d\n" "ifmt" builder in

        (* Construct function args as local vars *)
        let add_local builder m (t, n) p =
            L.set_value_name n p;
            let local = L.build_alloca (ltype_of_typ t) n builder in
            ignore (L.build_store p local builder);
            StringMap.add n local m
        in
        let local_vars_args =
            let formals = List.fold_left2 (add_local builder) StringMap.empty fdecl.sformals
                    (Array.to_list (L.params the_function)) in
            formals
        in
        let local_vars = ref local_vars_args in

        (* Return the value for a variable or formal argument.
             Check local names first, then global names *)
        (* TODO dynamically scope for classes *)
        let lookup n = try StringMap.find n !local_vars
            with Not_found -> StringMap.find n global_vars
        in

        (* Construct code for an expression; return its value *)
        let rec build_expr builder ((_, e) : sexpr) =
            match e with
            | SIntLit i -> L.const_int i32_t i
            | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
            | SStrLit s -> L.build_global_stringptr s "tmp_str_lit" builder
            | SIdent s -> L.build_load (lookup s) s builder
            | SBinop (e1, op, e2) ->
                let e1' = build_expr builder e1
                and e2' = build_expr builder e2 in
                (
                    match op with
                    | Ast.Add -> L.build_add
                    | Ast.Sub -> L.build_sub
                    | Ast.Mul -> L.build_mul
                    | Ast.Div -> L.build_sdiv
                    | Ast.And -> L.build_and
                    | Ast.Or -> L.build_or
                    | Ast.Eq -> L.build_icmp L.Icmp.Eq
                    | Ast.Neq -> L.build_icmp L.Icmp.Ne
                    | Ast.Less -> L.build_icmp L.Icmp.Slt
                    | Ast.Greater -> L.build_icmp L.Icmp.Sgt
                ) e1' e2' "tmp" builder
            | SFunctcall ("MEOW", [e]) ->
                L.build_call printf_func [| str_format_str ; (build_expr builder e) |]
                    "printf" builder
            | SFunctcall (f, args) ->
                let (fdef, fdecl) = StringMap.find f function_decls in
                let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
                if fdecl.srtyp = Ast.Void then
                    L.build_call fdef (Array.of_list llargs) "" builder
                else
                    let result = f ^ "_result" in
                    L.build_call fdef (Array.of_list llargs) result builder
            | SClassFunctcall (inst, (f, args)) ->
                let v = lookup inst in
                let deref = inst ^ "_deref" in
                let v_deref = L.build_load v deref builder in
                let (fdef, fdecl) = StringMap.find f function_decls in
                let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
                if fdecl.srtyp = Ast.Void then
                    L.build_call fdef (Array.of_list (v_deref::llargs)) "" builder
                else
                    let result = f ^ "_result" in
                    L.build_call fdef (Array.of_list (v_deref::llargs)) result builder
            | SClassMemAccess (mem, inst, idx) ->
                let v = lookup inst in
                let access = inst ^ "_" ^ mem ^ "_access" in
                let deref = inst ^ "_deref" in
                let v_deref = L.build_load v deref builder in
                L.build_load (L.build_struct_gep v_deref idx access builder) access builder
            | _ -> raise (Unimplemented "unimplemented expression")
        in

        (* LLVM insists each basic block end with exactly one "terminator"
             instruction that transfers control.    This function runs "instr builder"
             if the current block does not already have a terminator.    Used,
             e.g., to handle the "fall off the end of the function" case. *)
        let add_terminal builder instr =
            match L.block_terminator (L.insertion_block builder) with
            | Some _ -> ()
            | None -> ignore (instr builder) in

        (* Build the code for the given statement; return the builder for
             the statement's successor (i.e., the next instruction will be built
             after the one generated by this call) *)
        let rec build_stmt builder = function
            | SExpr e -> ignore(build_expr builder e); builder
            | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
            | SBind b ->
                let t, _ = b in
                let () = local_vars := add_local builder !local_vars b (init_of_typ t) in
                builder
            | SBindAssign(b, e) ->
                let t, _ = b in
                let () = local_vars := add_local builder !local_vars b (build_expr builder e) in
                builder
            | SAssign(s, e) ->
                let v = lookup s in
                let _ = L.build_store (build_expr builder e) v builder in
                builder
            | SClassMemRassn(mem, inst, idx, e) ->
                let v = lookup inst in
                let deref = inst ^ "_deref" in
                let v_deref = L.build_load v deref builder in
                let access = inst ^ "_" ^ mem ^ "_access" in
                let new_v = build_expr builder e in
                let _ = L.build_store new_v (L.build_struct_gep v_deref idx access builder) builder in
                builder
            | SInstance(t, n) ->
                let ltype = ltype_of_typ t in
                let inst = L.build_alloca ltype n builder in
                match t with
                | TypIdent c ->
                    let cons_name = c ^ "_CONS" in
                    let cons, _ = StringMap.find cons_name function_decls in
                    let _ = L.build_call cons [| inst |] "" builder in
                    let () = local_vars := add_local builder !local_vars (Pointer t, n) inst in
                    builder
                | _ -> raise (Failure "should not come here")
            | _ -> raise (Unimplemented "unimplemented statement")
                (*
            | SIf (predicate, then_stmt, else_stmt) ->
                let bool_val = build_expr builder predicate in

                let then_bb = L.append_block context "then" the_function in
                ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
                let else_bb = L.append_block context "else" the_function in
                ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

                let end_bb = L.append_block context "if_end" the_function in
                let build_br_end = L.build_br end_bb in (* partial function *)
                add_terminal (L.builder_at_end context then_bb) build_br_end;
                add_terminal (L.builder_at_end context else_bb) build_br_end;

                ignore(L.build_cond_br bool_val then_bb else_bb builder);
                L.builder_at_end context end_bb

            | SWhile (predicate, body) ->
                let while_bb = L.append_block context "while" the_function in
                let build_br_while = L.build_br while_bb in (* partial function *)
                ignore (build_br_while builder);
                let while_builder = L.builder_at_end context while_bb in
                let bool_val = build_expr while_builder predicate in

                let body_bb = L.append_block context "while_body" the_function in
                add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

                let end_bb = L.append_block context "while_end" the_function in

                ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
                L.builder_at_end context end_bb
*)
        in
        (* Build the code for each statement in the function *)
        let func_builder = List.fold_left build_stmt builder fdecl.sbody in

        (* Add a return if the last block falls off the end *)
        if fdecl.srtyp = Ast.Void then
            add_terminal func_builder L.build_ret_void
        else
            add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
    in

    List.iter build_function_body all_functions;
    let () = (
        if !has_main then
            let _ = build_function_body main_fdecl in ()
        else ()
    ) in

    the_module
