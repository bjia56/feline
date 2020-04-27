module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

exception Unimplemented

let translate (p: program) (mod_name: string) =
    let context = L.global_context () in
    let the_module = L.create_module context mod_name in

    (* Helper to convert A type to L type *)
    let ltype_of_typ (t: A.typ) =
        match t with
        | A.Int -> L.i32_type context
        | A.Bool -> L.i1_type context
        | _ -> raise Unimplemented
    in

    let init_of_type (t: A.typ) =
        match t with
        | A.Int -> L.const_int (ltype_of_typ t) 0
        | A.Bool -> L.const_int (ltype_of_typ t) 0
        | _ -> raise Unimplemented
    in

    (* Construct global vars *)
    let global_vars : L.llvalue StringMap.t =
        let global_var m (t, n) =
            let init = init_of_type t
            in StringMap.add n (L.define_global n init the_module) m in
        List.fold_left global_var StringMap.empty p.sglobals in

    (* Define printf *)
    let printf_t : L.lltype =
        L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func : L.llvalue =
        L.declare_function "printf" printf_t the_module in

    (* Construct global funcs *)
    let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
        let function_decl m fdecl =
            let name = fdecl.sfname
            and formal_types =
                Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
            in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
            StringMap.add name (L.define_function name ftype the_module, fdecl) m in
        List.fold_left function_decl StringMap.empty p.sfunctions in

    (* Construct function body statements *)
    let build_function_body fdecl =
        let (the_function, _) = StringMap.find fdecl.sfname function_decls in
        let builder = L.builder_at_end context (L.entry_block the_function) in

        let str_format_str = L.build_global_stringptr "%s\n" "sfmt" builder in
        let int_format_str = L.build_global_stringptr "%d\n" "ifmt" builder in

        (* Construct function args as local vars *)
        let add_local m (t, n) p =
            L.set_value_name n p;
            let local = L.build_alloca (ltype_of_typ t) n builder in
            ignore (L.build_store p local builder);
            StringMap.add n local m
        in
        let local_vars_args =
            let formals = List.fold_left2 add_local StringMap.empty fdecl.sformals
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
            | SIdent s -> L.build_load (lookup s) s builder
            | SBinop (e1, op, e2) ->
                let e1' = build_expr builder e1
                and e2' = build_expr builder e2 in
                (
                    match op with
                    | A.Add -> L.build_add
                    | A.Sub -> L.build_sub
                    | A.And -> L.build_and
                    | A.Or -> L.build_or
                    | A.Equal -> L.build_icmp L.Icmp.Eq
                    | A.Neq -> L.build_icmp L.Icmp.Ne
                    | A.Less -> L.build_icmp L.Icmp.Slt
                ) e1' e2' "tmp" builder
            | SCall ("MEOW", [e]) ->
                L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
                    "printf" builder
            | SCall (f, args) ->
                let (fdef, fdecl) = StringMap.find f function_decls in
                let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
                let result = f ^ "_result" in
                L.build_call fdef (Array.of_list llargs) result builder
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
                let () = local_vars := add_local !local_vars b (init_of_typ t) in
                ()
            | SBindAssign b, e ->
                let t, _ = b in
                let () = local_vars := add_local !local_vars b (build_expr builder e) in
                ()
            | _ -> raise Unimplemented
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
        let func_builder = build_stmt builder (SBlock fdecl.sbody) in

        (* Add a return if the last block falls off the end *)
        add_terminal func_builder (L.build_ret (L.const_int i32_t 0))

    in

    List.iter build_function_body functions;
    the_module
