(* Semantic checking for Feline *)

open Ast
open Sast
open Printf
module StringMap = Map.Make (String)

(* Semantic checking of AST. Returns SAST if successful, otherwise
   throws an exception.

   Check each global variable, then check functions, then check classes *)
let rec check_module (all_modules : module_decl StringMap.t)
    (visited : bool StringMap.t) (name : string) (m : module_decl) =
  let () =
    if StringMap.mem name visited then
      raise (Failure ("circular import detected in module " ^ name))
    else ()
  in
  let visited = StringMap.add name true visited in

  let classes, functions, globals = (m.classes, m.functions, m.globals) in

  (* Collect imports *)
  let import_classes, import_functions, import_globals, simport_classes =
    let check_import_dups (kind : string) (imports : string list) =
      let rec dups = function
        | [] -> ()
        | n1 :: n2 :: _ when n1 = n2 ->
            raise (Failure ("duplicate import " ^ kind ^ ": " ^ n1))
        | _ :: t -> dups t
      in
      dups (List.sort compare imports)
    in
    let check_valid_import (import : string) =
      if StringMap.mem import all_modules then ()
      else raise (Failure ("cannot find module " ^ import))
    in
    let collect_imports (classes, functions, globals, sclasses) mod_name =
      let m = StringMap.find mod_name all_modules in
      let sast = check_module all_modules visited mod_name m in
      ( classes @ m.classes,
        functions @ m.functions,
        globals @ m.globals,
        sclasses @ sast.sclasses )
    in

    (* Check for duplicate imports *)
    check_import_dups "module" m.imports;

    (* Inject globalBuiltins module *)
    let mimports = if name <> "globalBuiltins" then "globalBuiltins" :: m.imports else m.imports in

    (* Check all imports are valid *)
    let _ = List.map check_valid_import mimports in

    (* Collect all fields declared in imports *)
    let c, f, g, sc =
      List.fold_left collect_imports ([], [], [], []) mimports
    in

    (* Check dups in imports *)
    check_import_dups "class" (List.map (fun c -> c.cname) c);
    check_import_dups "function" (List.map (fun f -> f.fname) f);
    check_import_dups "global" (List.map (fun (_, n) -> n) g);

    (c, f, g, sc)
  in

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
      | [] -> ()
      | (_, n1) :: (_, n2) :: _ when n1 = n2 ->
          raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in
    dups (List.sort (fun (_, a) (_, b) -> compare a b) binds)
  in

  (* Function to check if a global is a redefinition of a imported global *)
  let check_redefined_import_global map (_, n) =
    if StringMap.mem n map then
      raise
        (Failure ("global " ^ n ^ " has been imported and may not be redefined"))
    else StringMap.add n true map
  in

  (* Check that no globals are duplicate *)
  check_binds "global" globals;
  let _ =
    List.fold_left check_redefined_import_global StringMap.empty globals
  in

  (* Build symbol table for imported functions *)
  let import_func_map =
    List.fold_left
      (fun m f -> StringMap.add f.fname f m)
      StringMap.empty import_functions
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let imported_err =
      "function " ^ fd.fname ^ " has been imported and may not be redefined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *) in
    match fd with
    (* No duplicate functions or redefinitions of built-ins *)
    | _ when StringMap.mem n import_func_map -> make_err imported_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func import_func_map functions in

  (* Build symbol table for imported classes *)
  let import_class_map =
    List.fold_left
      (fun m c -> StringMap.add c.cname c m)
      StringMap.empty import_classes
  in

  (* Add class name symbol table *)
  let add_class map cd =
    let imported_err =
      "class " ^ cd.cname ^ " has been imported and may not be redefined"
    in
    let dup_err = "duplicate class " ^ cd.cname
    and make_err er = raise (Failure er)
    and n = cd.cname (* Name of the class *) in
    match cd with
    (* No duplicate classes *)
    | _ when StringMap.mem n import_class_map -> make_err imported_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n cd map
  in

  (* Collect all class names into one symbol table *)
  let class_decls = List.fold_left add_class import_class_map classes in

  (* Return a function from our function_decl symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  (* Return a class from our class_decl symbol table *)
  let find_class s =
    try StringMap.find s class_decls
    with Not_found -> raise (Failure ("unrecognized class " ^ s))
  in

  let ud_type_to_str typ =
    match typ with
    | TypIdent s -> s
    | _ -> raise (Failure "User defined type is invalid")
  in

  (* Return a public function from a class *)
  let find_pub_func f classd =
    let pred = function a when a.fname = f -> true | _ -> false in
    try List.find pred classd.pubfuncs
    with Not_found ->
      raise
        (Failure
           ("can't find public function " ^ f ^ " in class " ^ classd.cname))
  in

  (* Return a private function from a class *)
  let find_priv_func f classd =
    let pred = function a when a.fname = f -> true | _ -> false in
    try List.find pred classd.privfuncs
    with Not_found -> raise (Failure ("can't find private function " ^ f))
  in

  (* Don't necessarily need a main function *)
  (* let _ = find_func "MAIN" in *)
  let check_func func =
    (* Make sure no formals are void or duplicates *)
    check_binds "formal" func.formals;
    let locals = [] in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
        if lvaluet = rvaluet then lvaluet
      else
        raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols =
      List.fold_left
        (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty
        (import_globals @ globals @ func.formals)
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s map =
      try StringMap.find s map
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a public member from a class *)
    let find_pub_mem m classd =
      let pred = function
        (* Make sure both type and var name match *)
        | ty, name
          when (* ty = type_of_identifier m map
                  && *)
               name = m ->
            true
        | _ -> false
      in
      try List.find pred classd.pubmembers
      with Not_found ->
        raise
          (Failure ("no such public member " ^ m ^ " in class " ^ classd.cname))
    in
    let find_mem_idx m classd =
      let count = ref 0 in
      let pred x =
        let () = count := !count + 1 in
        match x with
        (* Make sure both type and var name match *)
        | ty, name
          when (* ty = type_of_identifier m symbols
                  && *)
               name = m ->
            true
        | _ -> false
      in
      try
        let _ =
          List.find pred
            (List.rev_append (List.rev classd.pubmembers) classd.privmembers)
        in
        !count - 1
      with Not_found ->
        raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
    in

    (* Return a semantically-checked expression (sexpr) , i.e. with a type *)
    let rec check_expr expr sym_tbl =
      match expr with
      | NullLit -> ((Null, SNullLit), sym_tbl)
      | IntLit l -> ((Int, SIntLit l), sym_tbl)
      | BoolLit l -> ((Bool, SBoolLit l), sym_tbl)
      | StrLit l -> ((TypIdent "STRIN", SStrLit l), sym_tbl)
      | Ident var -> ((type_of_identifier var sym_tbl, SIdent var), sym_tbl)
      | Binop (e1, op, e2) ->
          let (t1, e1'), sym_tbl = check_expr e1 sym_tbl in
          let (t2, e2'), sym_tbl = check_expr e2 sym_tbl in
          (* TODO: Make error more specific with pretty-printing functions *)
          let err = "illegal binary operator " in
          (* All binary operators require operands of the same type *)
          if t1 = t2 then
            (* Determine expression type based on operator and operator types *)
            let t =
              match op with
              | (Add | Sub | Mul | Div) when t1 = Int -> Int
              | (Eq | Neq) -> Bool
              | Less when t1 = Int -> Bool
              | Greater when t1 = Int -> Bool
              | (And | Or) when t1 = Bool -> Bool
              | _ -> raise (Failure err)
            in
            ((t, SBinop ((t1, e1'), op, (t2, e2'))), sym_tbl)
          else raise (Failure err)
      | Unop (op, e1) ->
          let (t1, e1'), m1 = check_expr e1 sym_tbl in
          (* Single unary operator must be not *)
          let t = match op with Not -> Bool in
          ((t, SUnop (op, (t1, e1'))), m1)
      | Functcall (fname, args) ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise
              (Failure
                 ( "expecting " ^ string_of_int param_length
                 ^ " arguments in function call" ))
          else
            let check_call (ft, _) e =
              let (et, e'), sym_tbl = check_expr e sym_tbl in
              let err = "illegal argument found in function call" in
              ((check_assign ft et err, e'), sym_tbl)
            in
            let args' = List.map2 check_call fd.formals args in
            let args'' = List.map (fun (a, b) -> a) args' in
            ((fd.rtyp, SFunctcall (fname, args'')), sym_tbl)
      | NewInstance cls_name ->
          (* Check if it is a valid class *)
          let _ = find_class cls_name in
          ((TypIdent cls_name, SNewInstance cls_name), sym_tbl)
      (* TODO: Need to account for ClassFunctcall and ClassMemAccess *)
      | ClassFunctcall (instance, (fname, args)) ->
          (* Check that the object has been instantiated *)
          (* Check that fname is a valid public function in class *)
          let cls =
            find_class (ud_type_to_str (type_of_identifier instance sym_tbl))
          in
          let fd = find_pub_func fname cls in
          let param_length = List.length fd.formals in
          (* Check that number of arguments is correct *)
          if List.length args != param_length then
            raise
              (Failure
                 ( "expecting " ^ string_of_int param_length
                 ^ " arguments in function call" ))
          else
            let check_call (ft, _) e =
              let (et, e'), sym_tbl = check_expr e sym_tbl in
              let err = "illegal argument found in function call" in
              ((check_assign ft et err, e'), sym_tbl)
            in
            (* Check that type of arguments is correct *)
            let args' = List.map2 check_call fd.formals args in
            let args'' = List.map (fun (a, b) -> a) args' in
            (* Return format *)
            (* ( ( typ , SClassFunctcall( string, (string, sexpr list ) ) ), sym_tbl) *)
            ( ( fd.rtyp,
                SClassFunctcall (instance, (cls.cname ^ "_" ^ fname, args'')) ),
              sym_tbl )
      | ClassMemAccess (mem, instance) ->
          (* Check that the object has been instantiated *)
          let instance_type = type_of_identifier instance sym_tbl in
          (* Check that mem name is a valid public member in class *)
          let c = find_class (ud_type_to_str instance_type) in
          let _ = find_pub_mem mem c in
          (* Return format *)
          (* (( typ, SClassMemAccess(string, string)), sym_tbl) *)
          ( (instance_type, SClassMemAccess (mem, instance, find_mem_idx mem c)),
            sym_tbl )
    in
   
		let check_bool_expr e sym_tbl =
		  let ((t, e'), m) = check_expr e sym_tbl in
		  match t with
		  | Bool -> (t, e')
		  | _ -> raise (Failure ("expected Boolean expression"))
		in 

    let rec check_stmt_list stmt_list locals symbols =
      match stmt_list with
      | [] -> []
      | s :: sl ->
          let st, lc, sy =
            check_stmt s locals symbols
            (* in let () = List.iter (printf "%s ") (List.map (fun (a, b) -> b) lc)  *)
          in
          st :: check_stmt_list sl lc sy
    and check_stmt stmt locals (* list of locals *) symbols (* symbol map *) =
      match stmt with
      | Expr e ->
          let (ty, str), tbl = check_expr e symbols in
          (SExpr (ty, str), locals, symbols)
      | Bind b ->
          let ty, name = b in
          let locals = locals @ [ (ty, name) ] in
          let symbols = StringMap.add name ty symbols in
          let () = check_binds "local" locals in
          (SBind (ty, name), locals, symbols)
      | BindAssign (b, e) ->
          let ty, name = b in
          let locals = locals @ [ (ty, name) ] in
          let symbols = StringMap.add name ty symbols in
          let () = check_binds "local" locals in
          let (typ, str), tbl = check_expr e symbols in
          let err = "illegal assignment in expression" in
          let _ = check_assign ty typ err in
          (SBindAssign ((ty, name), (typ, str)), locals, symbols)
      | Assign (var, e) ->
          let lt = type_of_identifier var symbols
          and (rt, e'), _ = check_expr e symbols in
          let err = "illegal assignment in expression" in
          let _ = check_assign lt rt err in
          (SAssign ((lt, var), (rt, e')), locals, symbols)
      | Return e ->
          let (t, e'), _ = check_expr e symbols in
          if t = func.rtyp then (SReturn (t, e'), locals, symbols)
          else raise (Failure "return gives incorrect type")
      (* TODO: Need to account for ClassMemRassn and Instance *)
      | ClassMemRassn (mem, instance, expr) ->
          (* Check that the object has been instantiated *)
          let instance_type = type_of_identifier instance symbols in
          (* Check that mem name is a valid public member in class *)
          let cls = find_class (ud_type_to_str instance_type) in
          let found_mem = find_pub_mem mem cls in
          let lt, _ = found_mem in
          let (rt, e'), _ = check_expr expr symbols in
          let err = "illegal assignment in expression" in
          let _ = check_assign lt rt err in
          (* Return type *)
          (* (SClassMemRassn(string, string, (typ, sx)), locals, symbols) *)
          ( SClassMemRassn ((lt, mem), instance, find_mem_idx mem cls, (rt, e')),
            locals,
            symbols )
      | Dealloc expr ->
          (* Check that the expression is of the correct type *)
          let (t, e'), _ = check_expr expr symbols in
          let () =
            match t with
            | TypIdent _ -> ()
            | _ ->
                raise
                  (Failure
                     "cannot apply delete statement to expression of \
                      non-object type")
          in
          (SDealloc (t, e'), locals, symbols)
       | If (e, sl) -> (SIf(check_bool_expr e symbols, check_stmt_list sl locals symbols), 
       					   locals,
       					   symbols)
       | IfElse (e, sl1, sl2) -> (SIfElse(check_bool_expr e symbols, check_stmt_list sl1 locals symbols,
       							 check_stmt_list sl2 locals symbols),
       							 locals,
       							 symbols)
       | While (e, sl) -> (SWhile(check_bool_expr e symbols, check_stmt_list sl locals symbols),
       							 locals,
       							 symbols)

    in
    (* body of check_func *)
    {
      srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      sbody = check_stmt_list func.body locals symbols;
    }
  in

  (* Check a class function *)
  let check_class_func (calling_class, func) =
    check_binds "formal" func.formals;
    let locals = [] in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet
        else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols =
      List.fold_left
        (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty
        ( import_globals @ globals @ calling_class.pubmembers
        @ calling_class.privmembers @ func.formals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s map =
      try StringMap.find s map
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a public member from a class *)
    let find_pub_mem m classd =
      let pred = function
        (* Make sure both type and var name match *)
        | ty, name
          when (* ty = type_of_identifier m symbols
                  && *)
               name = m ->
            true
        | _ -> false
      in
      try List.find pred classd.pubmembers
      with Not_found ->
        raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
    in
    let find_mem_idx m classd =
      let count = ref 0 in
      let pred x =
        let () = count := !count + 1 in
        match x with
        (* Make sure both type and var name match *)
        | ty, name
          when (* ty = type_of_identifier m symbols
                  && *)
               name = m ->
            true
        | _ -> false
      in
      try
        let _ =
          List.find pred
            (List.rev_append (List.rev classd.pubmembers) classd.privmembers)
        in
        !count - 1
      with Not_found ->
        raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
    in

    (* Return a private member from a class *)
    let find_priv_mem m classd =
      let pred = function
        (* Make sure both type and var name match *)
        | ty, name
          when (* ty = type_of_identifier m symbols
                  && *)
               name = m ->
            true
        | _ -> false
      in
      try List.find pred classd.privmembers
      with Not_found ->
        raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
    in

    (* Return a semantically-checked expression (sexpr) , i.e. with a type *)
    let rec check_expr expr sym_tbl =
      match expr with
      | NullLit -> ((Null, SNullLit), sym_tbl)
      | IntLit l -> ((Int, SIntLit l), sym_tbl)
      | BoolLit l -> ((Bool, SBoolLit l), sym_tbl)
      | StrLit l -> ((TypIdent "STRIN", SStrLit l), sym_tbl)
      | Ident var -> ((type_of_identifier var sym_tbl, SIdent var), sym_tbl)
      | Binop (e1, op, e2) ->
          let (t1, e1'), sym_tbl = check_expr e1 sym_tbl in
          let (t2, e2'), sym_tbl = check_expr e2 sym_tbl in
          (* TODO: Make error more specific with pretty-printing functions *)
          let err = "illegal binary operator " in
          (* All binary operators require operands of the same type *)
          if t1 = t2 then
            (* Determine expression type based on operator and operator types *)
            let t =
              match op with
              | (Add | Sub | Mul | Div) when t1 = Int -> Int
              | Eq | Neq -> Bool
              | Less when t1 = Int -> Bool
              | (And | Or) when t1 = Bool -> Bool
              | _ -> raise (Failure err)
            in
            ((t, SBinop ((t1, e1'), op, (t2, e2'))), sym_tbl)
          else raise (Failure err)
      | Unop (op, e1) ->
          let (t1, e1'), m1 = check_expr e1 sym_tbl in
          (* Single unary operator must be not *)
          let t = match op with Not -> Bool in
          ((t, SUnop (op, (t1, e1'))), m1)
      | Functcall (fname, args) ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise
              (Failure
                 ( "expecting " ^ string_of_int param_length
                 ^ " arguments in function call" ))
          else
            let check_call (ft, _) e =
              let (et, e'), sym_tbl = check_expr e sym_tbl in
              let err = "illegal argument found in function call" in
              ((check_assign ft et err, e'), sym_tbl)
            in
            let args' = List.map2 check_call fd.formals args in
            let args'' = List.map (fun (a, b) -> a) args' in
            ((fd.rtyp, SFunctcall (fname, args'')), sym_tbl)
      | NewInstance cls_name ->
          (* Check if it is a valid class *)
          let _ = find_class cls_name in
          ((TypIdent cls_name, SNewInstance cls_name), sym_tbl)
      (* TODO: Need to account for ClassFunctcall, ClassMemAccess *)
      | ClassFunctcall (instance, (fname, args)) ->
          let _ = "can't find public function " ^ fname in
          let fd =
            if instance = "DIS" then
              (* Calling a function within the class *)
              (* Check both public and private functions of class *)
              try find_pub_func fname calling_class
              with Failure err -> find_priv_func fname calling_class
            else
              (* Calling a function outside the class *)
              (* Check that the object has been instantiated within
                 the func scope *)
              (* Check that fname is a valid public function in class *)
              find_pub_func fname
                (find_class
                   (ud_type_to_str (type_of_identifier instance sym_tbl)))
          in
          let param_length = List.length fd.formals in
          (* Check that number of arguments is correct *)
          if List.length args != param_length then
            raise
              (Failure
                 ( "expecting " ^ string_of_int param_length
                 ^ " arguments in function call" ))
          else
            let check_call (ft, _) e =
              let (et, e'), sym_tbl = check_expr e sym_tbl in
              let err = "illegal argument found in function call" in
              ((check_assign ft et err, e'), sym_tbl)
            in
            (* Check that type of arguments is correct *)
            let args' = List.map2 check_call fd.formals args in
            let args'' = List.map (fun (a, b) -> a) args' in
            (* Return format *)
            (* ( ( typ , SClassFunctcall( string, (string, sexpr list ) ) ), sym_tbl) *)
          if instance = "DIS" then
            ( (fd.rtyp, SClassFunctcall (calling_class.cname, (fname, args''))),
              sym_tbl )
          else 
          	( (fd.rtyp, SClassFunctcall (instance, (fname, args''))),
              sym_tbl )
      | ClassMemAccess (mem, instance) ->
          let _ = "can't find public member " ^ mem in
          let found_mem, instance_type, instance_type_decl =
            if instance = "DIS" then
              (* Invoking a member within the class *)
              (* Check both public and private members of class *)
              let instance_type_decl = calling_class in
              try
                ( find_pub_mem mem instance_type_decl,
                  TypIdent instance_type_decl.cname,
                  instance_type_decl )
              with Failure err ->
                ( find_priv_mem mem instance_type_decl,
                  TypIdent instance_type_decl.cname,
                  instance_type_decl )
            else
              (* Invoking a member outside the class *)
              (* Check that the object has been instantiatiated *)
              let instance_type = type_of_identifier instance sym_tbl in
              let instance_type_decl =
                find_class (ud_type_to_str instance_type)
              in
              (* Check that the mem name is a valid public member in class *)
              ( find_pub_mem mem instance_type_decl,
                instance_type,
                instance_type_decl )
          in
          let ty, _ =
            found_mem
            (* Return format *)
            (* (( typ, SClassMemAccess(string, string)), sym_tbl) *)
          in
          ( ( ty,
              SClassMemAccess
                (mem, instance, find_mem_idx mem instance_type_decl) ),
            sym_tbl )
    in

    let check_bool_expr e sym_tbl =
		  let ((t, e'), m) = check_expr e sym_tbl in
		  match t with
		  | Bool -> (t, e')
		  | _ -> raise (Failure ("expected Boolean expression"))
		in 
    let rec check_stmt_list stmt_list locals symbols =
      match stmt_list with
      | [] -> []
      | s :: sl ->
          let st, lc, sy = check_stmt s locals symbols in
          st :: check_stmt_list sl lc sy
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt stmt locals (* list of locals *) symbols (* symbol map *) =
      match stmt with
      | Expr e ->
          let (ty, str), tbl = check_expr e symbols in
          (SExpr (ty, str), locals, symbols)
      | Bind b ->
          let ty, name = b in
          let locals = locals @ [ (ty, name) ] in
          let symbols = StringMap.add name ty symbols in
          let () = check_binds "local" locals in
          (SBind (ty, name), locals, symbols)
      | BindAssign (b, e) ->
          let ty, name = b in
          let locals = locals @ [ (ty, name) ] in
          let symbols = StringMap.add name ty symbols in
          let () = check_binds "local" locals in
          let (typ, str), tbl = check_expr e symbols in
          let err = "illegal assignment in expression" in
          let _ = check_assign ty typ err in
          (SBindAssign ((ty, name), (typ, str)), locals, symbols)
      | Assign (var, e) ->
          let lt = type_of_identifier var symbols
          and (rt, e'), _ = check_expr e symbols in
          let err = "illegal assignment in expression" in
          let _ = check_assign lt rt err in
          (SAssign ((lt, var), (rt, e')), locals, symbols)
      | Return e ->
          let (t, e'), _ = check_expr e symbols in
          if t = func.rtyp then (SReturn (t, e'), locals, symbols)
          else raise (Failure "return gives incorrect type")
      (* TODO: Need to account for ClassMemRassn and Instance *)
      | ClassMemRassn (mem, instance, expr) ->
          let _ = "can't find public member " ^ mem in
          let found_mem, instance_type, instance_type_decl =
            if instance = "DIS" then
              (* Invoking a member within the class *)
              (* Check both public and private members of class *)
              let instance_type_decl = calling_class in
              try
                ( find_pub_mem mem instance_type_decl,
                  TypIdent instance_type_decl.cname,
                  instance_type_decl )
              with Failure err ->
                ( find_priv_mem mem instance_type_decl,
                  TypIdent instance_type_decl.cname,
                  instance_type_decl )
            else
              (* Invoking a member outside the class *)
              (* Check that the object has been instantiatiated *)
              let instance_type = type_of_identifier instance symbols in
              let instance_type_decl =
                find_class (ud_type_to_str instance_type)
              in
              (* Check that the mem name is a valid public member in class *)
              ( find_pub_mem mem instance_type_decl,
                instance_type,
                instance_type_decl )
          in
          let (rt, e'), _ = check_expr expr symbols in
          let err = "illegal assignment in expression" in
          let lt, _ = found_mem in
          let _ = check_assign lt rt err in
          ( SClassMemRassn
              ((lt, mem), instance, find_mem_idx mem instance_type_decl, (rt, e')),
            locals,
            symbols )
      | Dealloc expr ->
          (* Check that the expression is of the correct type *)
          let (t, e'), _ = check_expr expr symbols in
          let () =
            match t with
            | TypIdent _ -> ()
            | _ ->
                raise
                  (Failure
                     "cannot apply delete statement to expression of \
                      non-object type")
          in
          (SDealloc (t, e'), locals, symbols)
       | If (e, sl) -> (SIf(check_bool_expr e symbols, check_stmt_list sl locals symbols), 
       					   locals,
       					   symbols)
       | IfElse (e, sl1, sl2) -> (SIfElse(check_bool_expr e symbols, check_stmt_list sl1 locals symbols,
       							 check_stmt_list sl2 locals symbols),
       							 locals,
       							 symbols)
       | While (e, sl) -> (SWhile(check_bool_expr e symbols, check_stmt_list sl locals symbols),
       							 locals,
       							 symbols)
    in
    (* body of check_func *)
    {
      srtyp = func.rtyp;
      sfname = calling_class.cname ^ "_" ^ func.fname;
      sformals = (TypIdent calling_class.cname, "DIS") :: func.formals;
      sbody = check_stmt_list func.body locals symbols;
    }
  in

  (* TODO: Check class constructor and destructor *)
  let check_cons (calling_class, cons) =
    let locals = [] in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet
      else
        raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols =
      List.fold_left
        (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty
        ( import_globals @ globals @ calling_class.pubmembers
        @ calling_class.privmembers )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s map =
      try StringMap.find s map
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a public member from a class *)
    let find_pub_mem m classd =
      let pred = function
        (* Make sure both type and var name match *)
        | ty, name
          when (* ty = type_of_identifier m symbols
                  && *)
               name = m ->
            true
        | _ -> false
      in
      try List.find pred classd.pubmembers
      with Not_found ->
        raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
    in
    let find_mem_idx m classd =
      let count = ref 0 in
      let pred x =
        let () = count := !count + 1 in
        match x with
        (* Make sure both type and var name match *)
        | ty, name
          when (* ty = type_of_identifier m symbols
                  && *)
               name = m ->
            true
        | _ -> false
      in
      try
        let _ =
          List.find pred
            (List.rev_append (List.rev classd.pubmembers) classd.privmembers)
        in
        !count - 1
      with Not_found ->
        raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
    in

    (* Return a private member from a class *)
    let find_priv_mem m classd =
      let pred = function
        (* Make sure both type and var name match *)
        | ty, name
          when (* ty = type_of_identifier m symbols
                  && *)
               name = m ->
            true
        | _ -> false
      in
      try List.find pred classd.privmembers
      with Not_found ->
        raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
    in

    (* Return a semantically-checked expression (sexpr) , i.e. with a type *)
    let rec check_expr expr sym_tbl =
      match expr with
      | NullLit -> ((Null, SNullLit), sym_tbl)
      | IntLit l -> ((Int, SIntLit l), sym_tbl)
      | BoolLit l -> ((Bool, SBoolLit l), sym_tbl)
      | StrLit l -> ((TypIdent "STRIN", SStrLit l), sym_tbl)
      | Ident var -> ((type_of_identifier var sym_tbl, SIdent var), sym_tbl)
      | Binop (e1, op, e2) ->
          let (t1, e1'), sym_tbl = check_expr e1 sym_tbl in
          let (t2, e2'), sym_tbl = check_expr e2 sym_tbl in
          (* TODO: Make error more specific with pretty-printing functions *)
          let err = "illegal binary operator " in
          (* All binary operators require operands of the same type *)
          if t1 = t2 then
            (* Determine expression type based on operator and operator types *)
            let t =
              match op with
              | (Add | Sub | Mul | Div) when t1 = Int -> Int
              | Eq | Neq -> Bool
              | Less when t1 = Int -> Bool
              | (And | Or) when t1 = Bool -> Bool
              | _ -> raise (Failure err)
            in
            ((t, SBinop ((t1, e1'), op, (t2, e2'))), sym_tbl)
          else raise (Failure err)
      | Unop (op, e1) ->
          let (t1, e1'), m1 = check_expr e1 sym_tbl in
          (* Single unary operator must be not *)
          let t = match op with Not -> Bool in
          ((t, SUnop (op, (t1, e1'))), m1)
      | Functcall (fname, args) ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise
              (Failure
                 ( "expecting " ^ string_of_int param_length
                 ^ " arguments in function call" ))
          else
            let check_call (ft, _) e =
              let (et, e'), sym_tbl = check_expr e sym_tbl in
              let err = "illegal argument found in function call" in
              ((check_assign ft et err, e'), sym_tbl)
            in
            let args' = List.map2 check_call fd.formals args in
            let args'' = List.map (fun (a, b) -> a) args' in
            ((fd.rtyp, SFunctcall (fname, args'')), sym_tbl)
      | NewInstance cls_name ->
          (* Check if it is a valid class *)
          let _ = find_class cls_name in
          ((TypIdent cls_name, SNewInstance cls_name), sym_tbl)
      (* TODO: Need to account for ClassFunctcall, ClassMemAccess *)
      | ClassFunctcall (instance, (fname, args)) ->
          let _ = "can't find public function " ^ fname in
          let fd =
            if instance = "DIS" then
              (* Calling a function within the class *)
              (* Check both public and private functions of class *)
              try find_pub_func fname calling_class
              with Failure err -> find_priv_func fname calling_class
            else
              (* Calling a function outside the class *)
              (* Check that the object has been instantiated within
                 the func scope *)
              (* Check that fname is a valid public function in class *)
              find_pub_func fname
                (find_class
                   (ud_type_to_str (type_of_identifier instance sym_tbl)))
          in
          let param_length = List.length fd.formals in
          (* Check that number of arguments is correct *)
          if List.length args != param_length then
            raise
              (Failure
                 ( "expecting " ^ string_of_int param_length
                 ^ " arguments in function call" ))
          else
            let check_call (ft, _) e =
              let (et, e'), sym_tbl = check_expr e sym_tbl in
              let err = "illegal argument found in function call" in
              ((check_assign ft et err, e'), sym_tbl)
            in
            (* Check that type of arguments is correct *)
            let args' = List.map2 check_call fd.formals args in
            let args'' = List.map (fun (a, b) -> a) args' in
            (* Return format *)
            (* ( ( typ , SClassFunctcall( string, (string, sexpr list ) ) ), sym_tbl) *)
          if instance = "DIS" then
            ( (fd.rtyp, SClassFunctcall (calling_class.cname, (fname, args''))),
              sym_tbl )
          else 
          	( (fd.rtyp, SClassFunctcall (instance, (fname, args''))),
              sym_tbl )
      | ClassMemAccess (mem, instance) ->
          let _ = "can't find public member " ^ mem in
          let found_mem, instance_type, instance_type_decl =
            if instance = "DIS" then
              (* Invoking a member within the class *)
              (* Check both public and private members of class *)
              let instance_type_decl = calling_class in
              try
                ( find_pub_mem mem instance_type_decl,
                  TypIdent instance_type_decl.cname,
                  instance_type_decl )
              with Failure err ->
                ( find_priv_mem mem instance_type_decl,
                  TypIdent instance_type_decl.cname,
                  instance_type_decl )
            else
              (* Invoking a member outside the class *)
              (* Check that the object has been instantiatiated *)
              let instance_type = type_of_identifier instance sym_tbl in
              let instance_type_decl =
                find_class (ud_type_to_str instance_type)
              in
              (* Check that the mem name is a valid public member in class *)
              ( find_pub_mem mem instance_type_decl,
                instance_type,
                instance_type_decl )
          in
          let ty, _ =
            found_mem
            (* Return format *)
            (* (( typ, SClassMemAccess(string, string)), sym_tbl) *)
          in
          ( ( ty,
              SClassMemAccess
                (mem, instance, find_mem_idx mem instance_type_decl) ),
            sym_tbl )
    in

    let check_bool_expr e sym_tbl=
         let ((t, e'), m) = check_expr e sym_tbl in
         match t with
         | Bool -> (t, e')
         | _ -> raise (Failure ("expected Boolean expression"))
       in
    let rec check_stmt_list stmt_list locals symbols =
      match stmt_list with
      | [] -> []
      | s :: sl ->
          let st, lc, sy = check_stmt s locals symbols in
          st :: check_stmt_list sl lc sy
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt stmt locals (* list of locals *) symbols (* symbol map *) =
      match stmt with
      | Expr e ->
          let (ty, str), tbl = check_expr e symbols in
          (SExpr (ty, str), locals, symbols)
      | Bind b ->
          let ty, name = b in
          let locals = locals @ [ (ty, name) ] in
          let symbols = StringMap.add name ty symbols in
          let () = check_binds "local" locals in
          (SBind (ty, name), locals, symbols)
      | BindAssign (b, e) ->
          let ty, name = b in
          let locals = locals @ [ (ty, name) ] in
          let symbols = StringMap.add name ty symbols in
          let () = check_binds "local" locals in
          let (typ, str), tbl = check_expr e symbols in
          let err = "illegal assignment in expression" in
          let _ = check_assign ty typ err in
          (SBindAssign ((ty, name), (typ, str)), locals, symbols)
      | Assign (var, e) ->
          let lt = type_of_identifier var symbols
          and (rt, e'), _ = check_expr e symbols in
          let err = "illegal assignment in expression" in
          let _ = check_assign lt rt err in
          (SAssign ((lt, var), (rt, e')), locals, symbols)
      (* | Return e ->
         let ((t, e'), _) = check_expr e symbols in
         if t = func.rtyp then (SReturn (t, e'), locals, symbols)
         else raise (
         	Failure ("return gives incorrect type")
         ) *)
      (* TODO: Need to account for ClassMemRassn and Instance *)
      | ClassMemRassn (mem, instance, expr) ->
          let _ = "can't find public member " ^ mem in
          let found_mem, instance_type, instance_type_decl =
            if instance = "DIS" then
              (* Invoking a member within the class *)
              (* Check both public and private members of class *)
              let instance_type_decl = calling_class in
              try
                ( find_pub_mem mem instance_type_decl,
                  TypIdent instance_type_decl.cname,
                  instance_type_decl )
              with Failure err ->
                ( find_priv_mem mem instance_type_decl,
                  TypIdent instance_type_decl.cname,
                  instance_type_decl )
            else
              (* Invoking a member outside the class *)
              (* Check that the object has been instantiatiated *)
              let instance_type = type_of_identifier instance symbols in
              let instance_type_decl =
                find_class (ud_type_to_str instance_type)
              in
              (* Check that the mem name is a valid public member in class *)
              ( find_pub_mem mem instance_type_decl,
                instance_type,
                instance_type_decl )
          in
          let (rt, e'), _ = check_expr expr symbols in
          let err = "illegal assignment in expression" in
          let lt, _ = found_mem in
          let _ = check_assign lt rt err in
          ( SClassMemRassn
              ((lt, mem), instance, find_mem_idx mem instance_type_decl, (rt, e')),
            locals,
            symbols )
      | Dealloc expr ->
          (* Check that the expression is of the correct type *)
          let (t, e'), _ = check_expr expr symbols in
          let () =
            match t with
            | TypIdent _ -> ()
            | _ ->
                raise
                  (Failure
                     "cannot apply delete statement to expression of \
                      non-object type")
          in
          (SDealloc (t, e'), locals, symbols)
       | If (e, sl) -> (SIf(check_bool_expr e symbols, check_stmt_list sl locals symbols), 
       					   locals,
       					   symbols)
       | IfElse (e, sl1, sl2) -> (SIfElse(check_bool_expr e symbols, check_stmt_list sl1 locals symbols,
       							 check_stmt_list sl2 locals symbols),
       							 locals,
       							 symbols)
       | While (e, sl) -> (SWhile(check_bool_expr e symbols, check_stmt_list sl locals symbols),
       							 locals,
       							 symbols)
    in
    check_stmt_list cons locals symbols
  in

  let check_des (calling_class, des) =
    let locals = [] in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet
      else
        raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols =
      List.fold_left
        (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty
        ( import_globals @ globals @ calling_class.pubmembers
        @ calling_class.privmembers )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s map =
      try StringMap.find s map
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a public member from a class *)
    let find_pub_mem m classd =
      let pred = function
        (* Make sure both type and var name match *)
        | ty, name
          when (* ty = type_of_identifier m symbols
                  && *)
               name = m ->
            true
        | _ -> false
      in
      try List.find pred classd.pubmembers
      with Not_found ->
        raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
    in
    let find_mem_idx m classd =
      let count = ref 0 in
      let pred x =
        let () = count := !count + 1 in
        match x with
        (* Make sure both type and var name match *)
        | ty, name
          when (* ty = type_of_identifier m symbols
                  && *)
               name = m ->
            true
        | _ -> false
      in
      try
        let _ =
          List.find pred
            (List.rev_append (List.rev classd.pubmembers) classd.privmembers)
        in
        !count - 1
      with Not_found ->
        raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
    in

    (* Return a private member from a class *)
    let find_priv_mem m classd =
      let pred = function
        (* Make sure both type and var name match *)
        | ty, name
          when (* ty = type_of_identifier m symbols
                  && *)
               name = m ->
            true
        | _ -> false
      in
      try List.find pred classd.privmembers
      with Not_found ->
        raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
    in

    (* Return a semantically-checked expression (sexpr) , i.e. with a type *)
    let rec check_expr expr sym_tbl =
      match expr with
      | NullLit -> ((Null, SNullLit), sym_tbl)
      | IntLit l -> ((Int, SIntLit l), sym_tbl)
      | BoolLit l -> ((Bool, SBoolLit l), sym_tbl)
      | StrLit l -> ((TypIdent "STRIN", SStrLit l), sym_tbl)
      | Ident var -> ((type_of_identifier var sym_tbl, SIdent var), sym_tbl)
      | Binop (e1, op, e2) ->
          let (t1, e1'), sym_tbl = check_expr e1 sym_tbl in
          let (t2, e2'), sym_tbl = check_expr e2 sym_tbl in
          (* TODO: Make error more specific with pretty-printing functions *)
          let err = "illegal binary operator " in
          (* All binary operators require operands of the same type *)
          if t1 = t2 then
            (* Determine expression type based on operator and operator types *)
            let t =
              match op with
              | (Add | Sub | Mul | Div) when t1 = Int -> Int
              | Eq | Neq -> Bool
              | Less when t1 = Int -> Bool
              | (And | Or) when t1 = Bool -> Bool
              | _ -> raise (Failure err)
            in
            ((t, SBinop ((t1, e1'), op, (t2, e2'))), sym_tbl)
          else raise (Failure err)
      | Unop (op, e1) ->
          let (t1, e1'), m1 = check_expr e1 sym_tbl in
          (* Single unary operator must be not *)
          let t = match op with Not -> Bool in
          ((t, SUnop (op, (t1, e1'))), m1)
      | Functcall (fname, args) ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise
              (Failure
                 ( "expecting " ^ string_of_int param_length
                 ^ " arguments in function call" ))
          else
            let check_call (ft, _) e =
              let (et, e'), sym_tbl = check_expr e sym_tbl in
              let err = "illegal argument found in function call" in
              ((check_assign ft et err, e'), sym_tbl)
            in
            let args' = List.map2 check_call fd.formals args in
            let args'' = List.map (fun (a, b) -> a) args' in
            ((fd.rtyp, SFunctcall (fname, args'')), sym_tbl)
      | NewInstance cls_name ->
          (* Check if it is a valid class *)
          let _ = find_class cls_name in
          ((TypIdent cls_name, SNewInstance cls_name), sym_tbl)
      (* TODO: Need to account for ClassFunctcall, ClassMemAccess *)
      | ClassFunctcall (instance, (fname, args)) ->
          let _ = "can't find public function " ^ fname in
          let fd =
            if instance = "DIS" then
              (* Calling a function within the class *)
              (* Check both public and private functions of class *)
              try find_pub_func fname calling_class
              with Failure err -> find_priv_func fname calling_class
            else
              (* Calling a function outside the class *)
              (* Check that the object has been instantiated within
                 the func scope *)
              (* Check that fname is a valid public function in class *)
              find_pub_func fname
                (find_class
                   (ud_type_to_str (type_of_identifier instance sym_tbl)))
          in
          let param_length = List.length fd.formals in
          (* Check that number of arguments is correct *)
          if List.length args != param_length then
            raise
              (Failure
                 ( "expecting " ^ string_of_int param_length
                 ^ " arguments in function call" ))
          else
            let check_call (ft, _) e =
              let (et, e'), sym_tbl = check_expr e sym_tbl in
              let err = "illegal argument found in function call" in
              ((check_assign ft et err, e'), sym_tbl)
            in
            (* Check that type of arguments is correct *)
            let args' = List.map2 check_call fd.formals args in
            let args'' = List.map (fun (a, b) -> a) args' in
            (* Return format *)
            (* ( ( typ , SClassFunctcall( string, (string, sexpr list ) ) ), sym_tbl) *)
          if instance = "DIS" then
            ( (fd.rtyp, SClassFunctcall (calling_class.cname, (fname, args''))),
              sym_tbl )
          else 
          	( (fd.rtyp, SClassFunctcall (instance, (fname, args''))),
              sym_tbl )
      | ClassMemAccess (mem, instance) ->
          let _ = "can't find public member " ^ mem in
          let found_mem, instance_type, instance_type_decl =
            if instance = "DIS" then
              (* Invoking a member within the class *)
              (* Check both public and private members of class *)
              let instance_type_decl = calling_class in
              try
                ( find_pub_mem mem instance_type_decl,
                  TypIdent instance_type_decl.cname,
                  instance_type_decl )
              with Failure err ->
                ( find_priv_mem mem instance_type_decl,
                  TypIdent instance_type_decl.cname,
                  instance_type_decl )
            else
              (* Invoking a member outside the class *)
              (* Check that the object has been instantiatiated *)
              let instance_type = type_of_identifier instance sym_tbl in
              let instance_type_decl =
                find_class (ud_type_to_str instance_type)
              in
              (* Check that the mem name is a valid public member in class *)
              ( find_pub_mem mem instance_type_decl,
                instance_type,
                instance_type_decl )
          in
          let ty, _ =
            found_mem
            (* Return format *)
            (* (( typ, SClassMemAccess(string, string)), sym_tbl) *)
          in
          ( ( ty,
              SClassMemAccess
                (mem, instance, find_mem_idx mem instance_type_decl) ),
            sym_tbl )
    in

    let check_bool_expr e sym_tbl =
		  let ((t, e'), m) = check_expr e sym_tbl in
		  match t with
		  | Bool -> (t, e')
		  | _ -> raise (Failure ("expected Boolean expression"))
		in 
    let rec check_stmt_list stmt_list locals symbols =
      match stmt_list with
      | [] -> []
      | s :: sl ->
          let st, lc, sy = check_stmt s locals symbols in
          st :: check_stmt_list sl lc sy
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt stmt locals (* list of locals *) symbols (* symbol map *) =
      match stmt with
      | Expr e ->
          let (ty, str), tbl = check_expr e symbols in
          (SExpr (ty, str), locals, symbols)
      | Bind b ->
          let ty, name = b in
          let locals = locals @ [ (ty, name) ] in
          let symbols = StringMap.add name ty symbols in
          let () = check_binds "local" locals in
          (SBind (ty, name), locals, symbols)
      | BindAssign (b, e) ->
          let ty, name = b in
          let locals = locals @ [ (ty, name) ] in
          let symbols = StringMap.add name ty symbols in
          let () = check_binds "local" locals in
          let (typ, str), tbl = check_expr e symbols in
          let err = "illegal assignment in expression" in
          let _ = check_assign ty typ err in
          (SBindAssign ((ty, name), (typ, str)), locals, symbols)
      | Assign (var, e) ->
          let lt = type_of_identifier var symbols
          and (rt, e'), _ = check_expr e symbols in
          let err = "illegal assignment in expression" in
          let _ = check_assign lt rt err in
          (SAssign ((lt, var), (rt, e')), locals, symbols)
      (* | Return e ->
         let ((t, e'), _) = check_expr e symbols in
         if t = func.rtyp then (SReturn (t, e'), locals, symbols)
         else raise (
         	Failure ("return gives incorrect type")
         ) *)
      (* TODO: Need to account for ClassMemRassn and Instance *)
      | ClassMemRassn (mem, instance, expr) ->
          let _ = "can't find public member " ^ mem in
          let found_mem, instance_type, instance_type_decl =
            if instance = "DIS" then
              (* Invoking a member within the class *)
              (* Check both public and private members of class *)
              let instance_type_decl = calling_class in
              try
                ( find_pub_mem mem instance_type_decl,
                  TypIdent instance_type_decl.cname,
                  instance_type_decl )
              with Failure err ->
                ( find_priv_mem mem instance_type_decl,
                  TypIdent instance_type_decl.cname,
                  instance_type_decl )
            else
              (* Invoking a member outside the class *)
              (* Check that the object has been instantiatiated *)
              let instance_type = type_of_identifier instance symbols in
              let instance_type_decl =
                find_class (ud_type_to_str instance_type)
              in
              (* Check that the mem name is a valid public member in class *)
              ( find_pub_mem mem instance_type_decl,
                instance_type,
                instance_type_decl )
          in
          let (rt, e'), _ = check_expr expr symbols in
          let err = "illegal assignment in expression" in
          let lt, _ = found_mem in
          let _ = check_assign lt rt err in
          ( SClassMemRassn
              ((lt, mem), instance, find_mem_idx mem instance_type_decl, (rt, e')),
            locals,
            symbols )
      | Dealloc expr ->
          (* Check that the expression is of the correct type *)
          let (t, e'), _ = check_expr expr symbols in
          let () =
            match t with
            | TypIdent _ -> ()
            | _ ->
                raise
                  (Failure
                     "cannot apply delete statement to expression of \
                      non-object type")
          in
          (SDealloc (t, e'), locals, symbols)
       | If (e, sl) -> (SIf(check_bool_expr e symbols, check_stmt_list sl locals symbols), 
       					   locals,
       					   symbols)
       | IfElse (e, sl1, sl2) -> (SIfElse(check_bool_expr e symbols, check_stmt_list sl1 locals symbols,
       							 check_stmt_list sl2 locals symbols),
       							 locals,
       							 symbols)
       | While (e, sl) -> (SWhile(check_bool_expr e symbols, check_stmt_list sl locals symbols),
       							 locals,
       							 symbols)
    in
    check_stmt_list des locals symbols
  in

  (* TODO: Check a class *)
  let check_class class_decl =
    check_binds "member" (class_decl.pubmembers @ class_decl.privmembers);

    let class_pubfunc_binds =
      List.map
        (fun f -> (TypIdent class_decl.cname, f.fname))
        class_decl.pubfuncs
    in
    let class_privfunc_binds =
      List.map
        (fun f -> (TypIdent class_decl.cname, f.fname))
        class_decl.privfuncs
    in

    check_binds "method" (class_pubfunc_binds @ class_privfunc_binds);

    let class_pubfuncs =
      List.map (fun f -> (class_decl, f)) class_decl.pubfuncs
    in
    let class_privfuncs =
      List.map (fun f -> (class_decl, f)) class_decl.privfuncs
    in
    let class_cons = List.map (fun f -> (class_decl, f)) class_decl.cons in
    let class_des = List.map (fun f -> (class_decl, f)) class_decl.des in

    let result_cons =
      if List.length class_cons > 1 then
        raise
          (Failure
             ("class " ^ class_decl.cname ^ " has too many constructors defined"))
      else if List.length class_cons = 0 then []
      else check_cons (List.hd class_cons)
    in
    let result_des =
      if List.length class_des > 1 then
        raise
          (Failure
             ("class " ^ class_decl.cname ^ " has too many destructors defined"))
      else if List.length class_des = 0 then []
      else check_des (List.hd class_des)
    in

    {
      scname = class_decl.cname;
      spubmembers = class_decl.pubmembers;
      sprivmembers = class_decl.privmembers;
      spubfuncs = List.map check_class_func class_pubfuncs;
      sprivfuncs = List.map check_class_func class_privfuncs;
      scons = result_cons;
      sdes = result_des;
    }
  in
  {
    sclasses = List.map check_class classes;
    sfunctions = List.map check_func functions;
    sglobals = globals;
    sclass_imports = simport_classes;
    sfunction_imports =
      List.map
        (fun f ->
          { srtyp = f.rtyp; sfname = f.fname; sformals = f.formals; sbody = [] })
        import_functions;
    sglobal_imports = import_globals;
  }
