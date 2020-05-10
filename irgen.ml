open Sast
open Ast
module L = Llvm
module StringMap = Map.Make(String)

exception Unimplemented of string

let main_fdecl =
  {
    srtyp = Int;
    sfname = "main";
    sformals = [
        (Int, "argc");
        (PtrAsInt, "argv");
    ];
    sbody = [
        SExpr (Void, SFunctcall ("main_load_args", [(Int, SIdent("argc")); (PtrAsInt, SIdent("argv"))]));
        SReturn (Int, SFunctcall ("MAIN", []));
    ];
  }

let translate (mod_name : string) (p : smodule) =
  let context = L.global_context () in
  let the_module = L.create_module context "feline" in
  let class_lltype_map : (L.lltype StringMap.t) ref = ref StringMap.empty in

  (* Get types from the context *)
  let i64_t = L.i64_type context
  and i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and void_t = L.void_type context in

  (* Helper to convert A type to L type *)
  let rec ltype_of_typ (t : typ) =
    try
      match t with
      | Int -> i32_t
      | Bool -> i1_t
      | Void -> void_t
      | TypIdent s -> L.pointer_type (StringMap.find s !class_lltype_map)
      | PtrAsInt -> i64_t
      | _ ->
          raise
            (Unimplemented
               ("cannot convert unimplemented type " ^ Utils.string_of_typ t))
    with Not_found ->
      raise (Failure ("error converting type " ^ Utils.string_of_typ t))
  in

  let init_of_typ (t : typ) =
    match t with
    | Int -> L.const_int (ltype_of_typ t) 0
    | Bool -> L.const_int (ltype_of_typ t) 0
    | TypIdent _ -> L.const_null (ltype_of_typ t)
    | _ -> raise (Unimplemented "cannot init unimplemented type")
  in

  (* Preload classes *)
  let _ =
    let class_decl () scname =
      (* Construct class type first *)
      let ctype = L.named_struct_type context scname in
      class_lltype_map := StringMap.add scname ctype !class_lltype_map
    in
    List.fold_left class_decl ()
      (List.map (fun c -> c.scname) (p.sclasses @ p.sclass_imports))
  in

  (* Construct global funcs *)
  let has_main = ref false in
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let m2 =
        if fdecl.sfname = "MAIN" then
          let () = has_main := true in
          (* Construct wrapper around "MAIN" *)
          let ftype =
              L.function_type (ltype_of_typ Int) (Array.of_list [ i32_t; i64_t ])
          in
          StringMap.add main_fdecl.sfname
            (L.define_function main_fdecl.sfname ftype the_module, main_fdecl)
            m
        else m
      in

      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m2
    in
    List.fold_left function_decl StringMap.empty p.sfunctions
  in

  (* Construct classes *)
  let ( (class_decls :
          (L.lltype * (L.llvalue * sfunc_decl) StringMap.t * sclass_decl)
          StringMap.t),
        (import_class_decls :
          (L.lltype * (L.llvalue * sfunc_decl) StringMap.t * sclass_decl)
          StringMap.t) ) =
    let class_decl is_imported m cdecl =
      (* Construct class type first *)
      let name = cdecl.scname in
      let members =
        List.map
          (fun (t, _) -> ltype_of_typ t)
          (List.rev_append (List.rev cdecl.spubmembers) cdecl.sprivmembers)
      in
      let ctype = StringMap.find name !class_lltype_map in
      let () = L.struct_set_body ctype (Array.of_list members) false in

      let () = class_lltype_map := StringMap.add name ctype !class_lltype_map in

      (* Construct class functions *)
      let class_func_decl m fdecl =
        let name = fdecl.sfname
        and formal_types =
          Array.of_list
            ( L.pointer_type ctype
            :: List.tl (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
            )
        in
        let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
        StringMap.add name
          ( ( if is_imported then L.declare_function name ftype the_module
            else L.define_function name ftype the_module ),
            fdecl )
          m
      in

      let class_cons_decl m consdecl =
        class_func_decl m
          {
            srtyp = Void;
            sfname = name ^ "_CONS";
            sformals = [ (TypIdent name, "DIS") ];
            sbody = consdecl;
          }
      in
      let class_des_decl m desdecl =
        class_func_decl m
          {
            srtyp = Void;
            sfname = name ^ "_DES";
            sformals = [ (TypIdent name, "DIS") ];
            sbody = desdecl;
          }
      in

      let cfuncs =
        List.fold_left class_func_decl StringMap.empty cdecl.spubfuncs
      in
      let cfuncs = List.fold_left class_func_decl cfuncs cdecl.sprivfuncs in
      let cfuncs = class_cons_decl cfuncs cdecl.scons in
      let cfuncs = class_des_decl cfuncs cdecl.sdes in

      StringMap.add name (ctype, cfuncs, cdecl) m
    in
    ( List.fold_left (class_decl false) StringMap.empty p.sclasses,
      List.fold_left (class_decl true) StringMap.empty p.sclass_imports )
  in

  (* Add class functions to global functions *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let funcs = ref function_decls in
    let class_handler cname (_, m, _) =
      let class_func_handler fname v = funcs := StringMap.add fname v !funcs in
      StringMap.iter class_func_handler m
    in
    StringMap.iter class_handler class_decls;
    !funcs
  in
  let curr_mod_functions =
    let funcs = ref p.sfunctions in
    let class_handler cname (_, m, _) =
      let class_func_handler fname (_, fdecl) = funcs := fdecl :: !funcs in
      StringMap.iter class_func_handler m
    in
    StringMap.iter class_handler class_decls;
    !funcs
  in

  (* Add imported functions and class functions to global functions *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.declare_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl function_decls p.sfunction_imports
  in
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let funcs = ref function_decls in
    let class_handler cname (_, m, _) =
      let class_func_handler fname v = funcs := StringMap.add fname v !funcs in
      StringMap.iter class_func_handler m
    in
    StringMap.iter class_handler import_class_decls;
    !funcs
  in

  (* Construct global vars *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = init_of_typ t in
      StringMap.add n (L.define_global n init the_module) m
    in
    List.fold_left global_var StringMap.empty p.sglobals
  in

  (* Define malloc *)
  let malloc_t : L.lltype = L.function_type i64_t [| i64_t |] in
  let malloc_func : L.llvalue =
    L.declare_function "malloc" malloc_t the_module
  in

  (* Define free *)
  let free_t : L.lltype = L.function_type void_t [| i64_t |] in
  let free_func : L.llvalue = L.declare_function "free" free_t the_module in

  (* Define STRIN conversion function *)
  let strin_from_cstring_t : L.lltype = L.function_type void_t [| ltype_of_typ (TypIdent "STRIN"); L.pointer_type i8_t |] in
  let strin_from_cstring_func : L.llvalue = L.declare_function "STRIN_from_cstring" strin_from_cstring_t the_module in

  (* Define ARG preload function *)
  let load_args_t : L.lltype = L.function_type void_t [| i32_t; i64_t |] in
  let load_args_func : L.llvalue = L.declare_function "main_load_args" load_args_t the_module in

  (* Define imported functions *)
  let _ =
    let declare_imported_func f = L.declare_function f.fname in
    1
  in

  (* Construct function body statements *)
  let build_function_body fdecl =
    print_endline(fdecl.sfname);
    let the_function, _ = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let add_local builder m (t, n) p=
      print_endline(n);
      let local = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p local builder);
      StringMap.add n local m
    in      
    (* Construct function args as local vars *)
    let add_formal builder m (t, n) p =
      print_endline("Adding formal " ^ n);
      L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p local builder);
      StringMap.add n local m
    in
    (*let local_vars_args =*)
      print_endline ("sformal lenght is " ^ string_of_int (List.length fdecl.sformals));
      let local_vars_args =
        List.fold_left2 (add_formal builder) StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function))
      (*in
      formals*)
    in
    print_endline ("local_vars_args cardinality is " ^ 
                   string_of_int (StringMap.cardinal local_vars_args));
    let local_vars : (L.llvalue StringMap.t) ref = ref local_vars_args in

    (* Return the value for a variable or formal argument.
         Check local names first, then global names *)
    (* TODO dynamically scope for classes *)
    print_endline ("local_vars cardinality is " ^
                  string_of_int (StringMap.cardinal !local_vars));

    let lookup n =
      print_endline (string_of_int (StringMap.cardinal !local_vars));
      print_endline (string_of_bool (StringMap.mem n !local_vars));      
      try StringMap.find n !local_vars
      with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((_, e) : sexpr) =
      match e with
      | SIntLit i -> print_endline("SIntLit called"); L.const_int i32_t i
      | SBoolLit b -> print_endline("SBoolLit called");
                      L.const_int i1_t (if b then 1 else 0)
      | SStrLit s ->
          print_endline("SStrLit called");
          let const_str = L.build_global_stringptr s "str_lit" builder in
          let new_strin = build_expr builder (TypIdent "STRIN", SNewInstance "STRIN") in
          let _ = L.build_call strin_from_cstring_func [| new_strin; const_str |] "" builder in
          new_strin
      | SIdent s -> print_endline("SIdent called");
                    print_endline("Looking up variable " ^ s);
                    L.build_load (lookup s) s builder
      | SBinop (e1, op, e2) ->
          print_endline("SBinop called");
          let e1' = build_expr builder e1 and e2' = build_expr builder e2 in
          ( match op with
          | Add -> L.build_add
          | Sub -> L.build_sub
          | Mul -> L.build_mul
          | Div -> L.build_sdiv
          | And -> L.build_and
          | Or -> L.build_or
          | Eq -> L.build_icmp L.Icmp.Eq
          | Neq -> L.build_icmp L.Icmp.Ne
          | Less -> L.build_icmp L.Icmp.Slt
          | Greater -> L.build_icmp L.Icmp.Sgt )
            e1' e2' "tmp" builder
      | SFunctcall ("main_load_args", args) ->
          let llargs =
            List.rev (List.map (build_expr builder) (List.rev args))
          in
          L.build_call load_args_func (Array.of_list llargs) "" builder
      | SFunctcall (f, args) ->
          print_endline("SFunctcall called");
          let fdef, fdecl = StringMap.find f function_decls in
          let llargs =
            List.rev (List.map (build_expr builder) (List.rev args))
          in
          if fdecl.srtyp = Void then
            L.build_call fdef (Array.of_list llargs) "" builder
          else
            let result = f ^ "_result" in
            L.build_call fdef (Array.of_list llargs) result builder
      | SNewInstance n ->
          print_endline("SNewInstance called");
          let pltype = ltype_of_typ (TypIdent n) in
          let ltype = L.element_type pltype in
          (* Compute size of struct and cast to i32 *)
          let size_of_val = L.size_of ltype in
          (* Call malloc *)
          let malloc_ret = n ^ "_malloc" in
          let malloc_ret_val =
            L.build_call malloc_func [| size_of_val |] malloc_ret builder
          in
          (* Cast pointer and store *)
          let malloc_ptr = n ^ "_malloc_ptr" in
          let inst =
            L.build_inttoptr malloc_ret_val pltype malloc_ptr builder
          in
          (* Call constructor *)
          let cons_name = n ^ "_CONS" in
          let cons, _ = StringMap.find cons_name function_decls in
          let _ = L.build_call cons [| inst |] "" builder in
          (* Return instance *)
          inst
      | SClassFunctcall (inst, (f, args)) ->
          print_endline("SClassFunctcall called");
          let v = lookup inst in
          let deref = inst ^ "_deref" in
          let v_deref = L.build_load v deref builder in
          let fdef, fdecl = StringMap.find f function_decls in
          let llargs =
            List.rev (List.map (build_expr builder) (List.rev args))
          in
          if fdecl.srtyp = Void then
            L.build_call fdef (Array.of_list (v_deref :: llargs)) "" builder
          else
            let result = f ^ "_result" in
            L.build_call fdef (Array.of_list (v_deref :: llargs)) result builder
      | SClassMemAccess (mem, inst, idx) ->
          print_endline("SClassMemAccess called");
          let v = lookup inst in
          let access = inst ^ "_" ^ mem ^ "_access" in
          let deref = inst ^ "_deref" in
          let v_deref = L.build_load v deref builder in
          L.build_load
            (L.build_struct_gep v_deref idx access builder)
            access builder
      | _ -> raise (Unimplemented "unimplemented expression")
    in

    (* LLVM insists each basic block end with exactly one "terminator"
         instruction that transfers control.    This function runs "instr builder"
         if the current block does not already have a terminator.    Used,
         e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in

    (* Build the code for the given statement; return the builder for
         the statement's successor (i.e., the next instruction will be built
         after the one generated by this call) *)
    let rec build_stmt builder = function
      | SExpr e ->
          print_endline("SExpr called");
          ignore (build_expr builder e);
          builder
      | SReturn e ->
          print_endline("SReturn called");          
          ignore (L.build_ret (build_expr builder e) builder);
          builder
      | SBind b ->
          print_endline("SBind called");
          let t, _ = b in
          
          let () =
            local_vars := add_local builder !local_vars b (init_of_typ t)
          in
          builder
      | SBindAssign (b, e) ->
          print_endline("SBindAssign called");  
          let () =
            local_vars := add_local builder !local_vars b (build_expr builder e)
          in
          builder
      | SAssign (b, e) ->
          print_endline("SAssign called"); 
          let _, n = b in
          let v = lookup n in
          let _ = L.build_store (build_expr builder e) v builder in
          builder
      | SClassMemRassn (b, inst, idx, e) ->
          print_endline("SClassMemRassn called"); 
          let _, mem = b in
          let v = lookup inst in
          let deref = inst ^ "_deref" in
          let v_deref = L.build_load v deref builder in
          let access = inst ^ "_" ^ mem ^ "_access" in
          let new_v = build_expr builder e in
          let _ =
            L.build_store new_v
              (L.build_struct_gep v_deref idx access builder)
              builder
          in
          builder
      | SDealloc e ->
          print_endline("Destructor called"); 
          let t, _ = e in
          let v = build_expr builder e in
          (* Call destructor *)
          let c =
            match t with
            | TypIdent c ->
                let des_name = c ^ "_DES" in
                let des, _ = StringMap.find des_name function_decls in
                L.build_call des [| v |] "" builder;
                c
            | _ -> raise (Failure "should not come here")
          in
          (* Cast pointer and store *)
          let free_intptr = c ^ "_free_intptr" in
          let free_intptr_val = L.build_ptrtoint v i64_t free_intptr builder in
          (* Call free *)
          let _ = L.build_call free_func [| free_intptr_val |] "" builder in
          builder

      | SIf (predicate, then_stmt) ->
          print_endline("Single If called"); 
          let bool_val = build_expr builder predicate in

          let then_bb = L.append_block context "then" the_function in
          ignore (List.fold_left build_stmt (L.builder_at_end context then_bb) then_stmt);

          let end_bb = L.append_block context "if_end" the_function in
          let build_br_end = L.build_br end_bb in (* partial function *)
          add_terminal (L.builder_at_end context then_bb) build_br_end;

          ignore(L.build_cond_br bool_val then_bb end_bb builder);
          L.builder_at_end context end_bb

      | SIfElse (predicate, then_stmt, else_stmt) ->
          print_endline("If&Else called"); 
          let bool_val = build_expr builder predicate in

          let then_bb = L.append_block context "then" the_function in
          ignore (List.fold_left build_stmt (L.builder_at_end context then_bb) then_stmt);
          let else_bb = L.append_block context "else" the_function in
          ignore (List.fold_left build_stmt (L.builder_at_end context else_bb) else_stmt);

          let end_bb = L.append_block context "if_end" the_function in
          let build_br_end = L.build_br end_bb in (* partial function *)
          add_terminal (L.builder_at_end context then_bb) build_br_end;
          add_terminal (L.builder_at_end context else_bb) build_br_end;

          ignore(L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
          print_endline("While called"); 
          let while_bb = L.append_block context "while" the_function in
          let build_br_while = L.build_br while_bb in (* partial function *)
          ignore (build_br_while builder);
          let while_builder = L.builder_at_end context while_bb in
          let bool_val = build_expr while_builder predicate in

          let body_bb = L.append_block context "while_body" the_function in
          add_terminal (List.fold_left build_stmt (L.builder_at_end context body_bb) body) build_br_while;

          let end_bb = L.append_block context "while_end" the_function in

          ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
          L.builder_at_end context end_bb

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
    print_endline("No. of statements in this function is " ^
                  string_of_int(List.length fdecl.sbody));
    let func_builder = List.fold_left build_stmt builder fdecl.sbody in

    (* Add a return if the last block falls off the end *)
    if fdecl.srtyp = Void then add_terminal func_builder L.build_ret_void
    else add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
  in
  print_endline( string_of_int (List.length curr_mod_functions));
  List.map (fun a -> print_endline (a.sfname)) curr_mod_functions;
  print_endline("-----------");
  List.iter build_function_body curr_mod_functions;
  let () =
    if !has_main then
      let _ = build_function_body main_fdecl in
      print_endline("there is no main function.");
      ()
    else ()
  in

  the_module
