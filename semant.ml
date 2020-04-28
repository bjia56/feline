(* Semantic checking for Feline *)

open Ast
open Sast
open Printf

module StringMap = Map.Make(String)


(* Semantic checking of AST. Returns SAST if successful, otherwise
   throws an exception.

   Check each global variable, then check functions, then check classes *)

let check (classes, functions, globals) =

	(* Verify a list of bindings has no duplicate names *)
	let check_binds (kind : string) (binds : (typ * string) list) =
      let rec dups = function
          [] -> ()
        | ((_,n1) :: (_,n2) :: _) when n1 = n2 ->
          raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
        | _ :: t -> dups t
      in dups (List. sort (fun (_,a) (_, b) -> compare a b) binds)
    in

    (* Check that no globals are duplicate *)
    check_binds "global" globals;

    (* Collect function declarations for built-in functions: no bodies *)
    let built_in_decls =
      StringMap.add "MEOW" {
      	rtyp = Int;
      	fname = "MEOW";
      	formals = [(String, "x")];
      	body = [] } StringMap.empty
    in

    (* Add function name to symbol table *)
    let add_func map fd =
    	let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    	and dup_err = "duplicate function " ^ fd.fname
    	and make_err er = raise (Failure er)
    	and n = fd.fname (* Name of the function *)
	    in match fd with (* No duplicate functions or redefinitions of built-ins *)
	      _ when StringMap.mem n built_in_decls -> make_err built_in_err
	    | _ when StringMap.mem n map -> make_err dup_err
	    | _ -> StringMap.add n fd map
	in

	(* Collect all function names into one symbol table *)
	let function_decls = List.fold_left add_func built_in_decls functions
	in

	(* Collect all class names into one symbol table *)
	let add_class map cd =
		let dup_err = "duplicate class " ^ cd.cname
		and make_err er = raise (Failure er)
		and n = cd.cname (* Name of the class *)
		in match cd with (* No duplicate classes *)
		  _ when StringMap.mem n map -> make_err dup_err
		| _ -> StringMap.add n cd map
	in

	(* Collect all class names into one symbol table *)
	let class_decls = List.fold_left add_class StringMap.empty classes
	in

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
		TypIdent s -> s
		| _ -> raise (Failure("User defined type is invalid"))
	in

	(* Return a public function from a class *)
	let find_pub_func f classd =
			let pred = function
				a when a.fname = f -> true
			  | _ -> false
			in
		try List.find pred classd.pubfuncs
		with Not_found -> raise (Failure ("can't find public function " ^ f
										   ^ " in class " ^ classd.cname  ))
	in

	(* Return a private function from a class *)
	let find_priv_func f classd =
			let pred = function
				a when a.fname = f-> true
			  | _ -> false
			in
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
		  if lvaluet = rvaluet then lvaluet else raise (Failure err)
		in

		(* Build local symbol table of variables for this function *)
		let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
			StringMap.empty (globals @ func.formals)
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
			  | (ty, name) when (* ty = type_of_identifier m map
			  					&& *) name = m -> true
			  | _ -> false
			in
			try List.find pred classd.pubmembers
			with Not_found -> raise (Failure ("no such public member " ^ m ^ " in class " ^ classd.cname))
		in

		(* Return a semantically-checked expression (sexpr) , i.e. with a type *)
		let rec check_expr expr sym_tbl =
		   match expr with
		  | NullLit -> ((Null, SNullLit), sym_tbl)
		  | IntLit l -> ((Int, SIntLit l), sym_tbl)
		  | BoolLit l -> ((Bool, SBoolLit l), sym_tbl)
		  | StrLit  l -> ((String, SStrLit l), sym_tbl)
		  | Ident var -> ((type_of_identifier var sym_tbl, SIdent var), sym_tbl)
		  | Binop(e1, op, e2) ->
		    let ((t1, e1'), sym_tbl) = check_expr e1 sym_tbl in
		    let ((t2, e2'), sym_tbl) = check_expr e2 sym_tbl in
		    (* TODO: Make error more specific with pretty-printing functions *)
		    let err = "illegal binary operator "
		  	in
		  	(* All binary operators require operands of the same type *)
		  	if t1 = t2 then
		  	   (* Determine expression type based on operator and operator types *)
		  	   let t = match op with
		  	       Add | Sub | Mul | Div when t1 = Int -> Int
		  	     | Eq | Neq -> Bool
		  	     | Less when t1 = Int -> Bool
		  	     | And | Or when t1 = Bool -> Bool
		  	     | _ -> raise (Failure err)
		  	    in
		  	    ((t, SBinop((t1, e1'), op, (t2, e2'))), sym_tbl)
		  	else raise (Failure err)
		  | Unop(op, e1) ->
		  	let ((t1, e1'), m1) = check_expr e1 sym_tbl in
			(* Single unary operator must be not *)
			let t = match op with
			  | Not -> Bool
			 in
			 ((t, SUnop(op, (t1, e1'))), m1)
		  | Functcall(fname, args) ->
		  	let fd = find_func fname in
		  	let param_length = List.length fd.formals in
		  	if List.length args != param_length then
		  		raise (Failure ("expecting " ^ string_of_int param_length ^
		  						" arguments in function call"))
		  	else let check_call (ft, _) e =
		  		   let ((et, e'), sym_tbl) = check_expr e sym_tbl in
		  		   let err = "illegal argument found in function call"
		  		   in ((check_assign ft et err, e'), sym_tbl)
		       in
		       let args' = List.map2 check_call fd.formals args in
		       let args'' = List.map (fun (a, b) -> a) args' in
		   	   ((fd.rtyp, SFunctcall(fname, args'')), sym_tbl)
		   	(* TODO: Need to account for ClassFunctcall and ClassMemAccess *)
	   	  | ClassFunctcall(fname, (instance, args)) ->
	   	    (* Check that the object has been instantiated *)
	   	    (* Check that fname is a valid public function in class *)
   		    let fd = find_pub_func fname (find_class (ud_type_to_str (type_of_identifier instance sym_tbl))) in
   		    let param_length = List.length fd.formals in
   		    (* Check that number of arguments is correct *)
   		    if List.length args != param_length then
   		      raise (Failure ("expecting " ^ string_of_int param_length ^
  						      " arguments in function call"))
   		    else let check_call (ft, _) e =
	  		   let ((et, e'), sym_tbl) = check_expr e sym_tbl in
	  		   let err = "illegal argument found in function call"
	  		   in ((check_assign ft et err, e'), sym_tbl)
	        in
	        (* Check that type of arguments is correct *)
	        let args' = List.map2 check_call fd.formals args in
	        let args'' = List.map (fun (a, b) -> a) args' in
	     	(* Return format *)
	     	(* ( ( typ , SClassFunctcall( string, (string, sexpr list ) ) ), sym_tbl) *)
		   	( ( fd.rtyp, SClassFunctcall (fname, (ud_type_to_str (type_of_identifier instance sym_tbl), args''))), sym_tbl)
	   	  | ClassMemAccess(mem, instance) ->
	   		(* Check that the object has been instantiated *)
	   		let instance_type = type_of_identifier instance sym_tbl in
	   		(* Check that mem name is a valid public member in class *)
	   		let _ = find_pub_mem mem ((find_class (ud_type_to_str instance_type))) in
	   		(* Return format *)
	   		(* (( typ, SClassMemAccess(string, string)), sym_tbl) *)
	   		((instance_type, SClassMemAccess(mem, instance)), sym_tbl)
		in
		(*
		let check_bool_expr e sym_tbl =
		  let ((t, e'), m) = check_expr e sym_tbl in
		  match t with
		  | Bool -> (t, e')
		  | _ -> raise (Failure ("expected Boolean expression"))
		in *)

		let rec check_stmt_list stmt_list locals symbols =
		  match stmt_list with
		    [] -> []
		  | s :: sl -> let (st, lc, sy) = check_stmt s locals symbols
						(* in let () = List.iter (printf "%s ") (List.map (fun (a, b) -> b) lc)  *)
						in st :: check_stmt_list sl lc sy
		and check_stmt stmt locals (* list of locals *) symbols (* symbol map *) =
		    match stmt with
		    Expr e ->
		    	 let ((ty, str), tbl) = check_expr e symbols in (SExpr(ty, str), locals, symbols)
		   | Bind b ->
			     let (ty, name) = b in
			     let locals = locals @ [(ty, name)] in
			     let symbols = StringMap.add name ty symbols in
			     let () = check_binds "local" locals in
		         (SExpr(ty, SIdent(name)), locals, symbols)
		   | BindAssign (b, e) ->
		         let (ty, name) = b in
		         let locals = locals @ [(ty, name)] in
		         let symbols = StringMap.add name ty symbols in
		         let () = check_binds "local" locals in
		         let ((typ, str), tbl) = check_expr e symbols in
		         (SBindAssign((ty, name), (typ, str)), locals, symbols)
		   | Assign(var, e) ->
			   	 let lt = type_of_identifier var symbols
			   	 and ((rt, e'), _) = check_expr e symbols in
			   	 let err = "illegal assignment in expression" in
			   	 let _ = check_assign lt rt err in
			   	 (SAssign(var, (rt, e')), locals, symbols)
		   | Return e ->
			     let ((t, e'), _) = check_expr e symbols in
			     if t = func.rtyp then (SReturn (t, e'), locals, symbols)
			     else raise (
			     	Failure ("return gives incorrect type")
			     )
		   (* TODO: Need to account for ClassMemRassn and Instance *)
		   | ClassMemRassn(mem, instance, expr) ->
			     (* Check that the object has been instantiated *)
			     let instance_type = type_of_identifier instance symbols in
			     (* Check that mem name is a valid public member in class *)
			   	 let found_mem = find_pub_mem mem (find_class (ud_type_to_str instance_type)) in
			     let (lt, _) = found_mem in
			     let ((rt, e'), _) = check_expr expr symbols in
			     let err = "illegal assignment in expression" in
			     let _ = check_assign lt rt err in
			     (* Return type *)
			     (* (SClassMemRassn(string, string, (typ, sx)), locals, symbols) *)
			   	 (SClassMemRassn(mem, instance, (rt, e')), locals, symbols)
		   | Instance (ud_type, name) ->
		   		 (* Make sure ud_type is a valid class *)
		   		 let _ = find_class (ud_type_to_str ud_type) in
		   		 (* Add bind to local list and to symbol table *)
			     let locals = locals @ [(ud_type, name)] in
			     let symbols = StringMap.add name ud_type symbols in
			     let () = check_binds "local" locals in
			     (SInstance(ud_type, name), locals, symbols)
		in (* body of check_func *)
		{ srtyp = func.rtyp;
		  sfname = func.fname;
		  sformals = func.formals;
		  sbody = check_stmt_list func.body locals symbols
	    }

	in

	(* Check a class function *)
	let check_class_func (calling_class, func) =

		check_binds "formal" func.formals;
		let locals = [] in

		(* Raise an exception if the given rvalue type cannot be assigned to
		   the given lvalue type *)
		let check_assign lvaluet rvaluet err =
		  if lvaluet = rvaluet then lvaluet else raise (Failure err)
		in

		(* Build local symbol table of variables for this function *)
		let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
			StringMap.empty (globals @ calling_class.pubmembers
							 @ calling_class.privmembers
							 @ func.formals )
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
				| (ty, name) when (* ty = type_of_identifier m symbols
								  &&  *) name = m -> true
				| _ -> false
			in
			try List.find pred classd.pubmembers
			with Not_found -> raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
		in

		(* Return a private member from a class *)
		let find_priv_mem m classd =
			let pred = function
				(* Make sure both type and var name match *)
				| (ty, name) when (* ty = type_of_identifier m symbols
								  && *) name = m -> true
				| _ -> false
			in
			try List.find pred classd.privmembers
			with Not_found -> raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
		in

		(* Return a semantically-checked expression (sexpr) , i.e. with a type *)
		let rec check_expr expr sym_tbl =
		   match expr with
		  | NullLit -> ((Null, SNullLit), sym_tbl)
		  | IntLit l -> ((Int, SIntLit l), sym_tbl)
		  | BoolLit l -> ((Bool, SBoolLit l), sym_tbl)
		  | StrLit  l -> ((String, SStrLit l), sym_tbl)
		  | Ident var -> ((type_of_identifier var sym_tbl, SIdent var), sym_tbl)
		  | Binop(e1, op, e2) ->
		    let ((t1, e1'), sym_tbl) = check_expr e1 sym_tbl in
		    let ((t2, e2'), sym_tbl) = check_expr e2 sym_tbl in
		    (* TODO: Make error more specific with pretty-printing functions *)
		    let err = "illegal binary operator "
		  	in
		  	(* All binary operators require operands of the same type *)
		  	if t1 = t2 then
		  	   (* Determine expression type based on operator and operator types *)
		  	   let t = match op with
		  	       Add | Sub | Mul | Div when t1 = Int -> Int
		  	     | Eq | Neq -> Bool
		  	     | Less when t1 = Int -> Bool
		  	     | And | Or when t1 = Bool -> Bool
		  	     | _ -> raise (Failure err)
		  	    in
		  	    ((t, SBinop((t1, e1'), op, (t2, e2'))), sym_tbl)
		  	else raise (Failure err)
		  | Unop(op, e1) ->
		  	let ((t1, e1'), m1) = check_expr e1 sym_tbl in
			(* Single unary operator must be not *)
			let t = match op with
			  | Not -> Bool
			 in
			 ((t, SUnop(op, (t1, e1'))), m1)
		  | Functcall(fname, args) ->
		  	let fd = find_func fname in
		  	let param_length = List.length fd.formals in
		  	if List.length args != param_length then
		  		raise (Failure ("expecting " ^ string_of_int param_length ^
		  						" arguments in function call"))
		  	else let check_call (ft, _) e =
		  		   let ((et, e'), sym_tbl) = check_expr e sym_tbl in
		  		   let err = "illegal argument found in function call"
		  		   in ((check_assign ft et err, e'), sym_tbl)
		       in
		       let args' = List.map2 check_call fd.formals args in
		       let args'' = List.map (fun (a, b) -> a) args' in
		   	   ((fd.rtyp, SFunctcall(fname, args'')), sym_tbl)
		   	(* TODO: Need to account for ClassFunctcall, ClassMemAccess *)
		   | ClassFunctcall(fname, (instance, args)) ->
		   		let _ = "can't find public function " ^ fname in
		   		let fd =
			   		if instance = "DIS" then
			   		        (* Calling a function within the class *)
			   				(* Check both public and private functions of class *)
			   				try
			   					find_pub_func fname calling_class
			   				with Failure(err) ->
			   					find_priv_func fname calling_class

			   		else   (* Calling a function outside the class *)
			   			   (* Check that the object has been instantiated within
		   					the func scope *)
		   				   (* Check that fname is a valid public function in class *)
			   				find_pub_func fname (find_class (ud_type_to_str (type_of_identifier instance sym_tbl)))
			   		in
			   	let param_length = List.length fd.formals in
		   		  (* Check that number of arguments is correct *)
		   		  if List.length args != param_length then
		   		      raise (Failure ("expecting " ^ string_of_int param_length ^
		  						      " arguments in function call"))
		   		  else let check_call (ft, _) e =
			  		   let ((et, e'), sym_tbl) = check_expr e sym_tbl in
			  		   let err = "illegal argument found in function call"
			  		   in ((check_assign ft et err, e'), sym_tbl)
			       in
			       (* Check that type of arguments is correct *)
			       let args' = List.map2 check_call fd.formals args in
			       let args'' = List.map (fun (a, b) -> a) args' in
			       (* Return format *)
			       (* ( ( typ , SClassFunctcall( string, (string, sexpr list ) ) ), sym_tbl) *)
				   ( ( fd.rtyp, SClassFunctcall (fname, (calling_class.cname, args''))), sym_tbl)
		   | ClassMemAccess(mem, instance) ->
		        let _ = "can't find public member " ^ mem in
		   		let (found_mem, instance_type) =
		   			if instance = "DIS" then
		   			         (* Invoking a member within the class *)
		   					 (* Check both public and private members of class *)
		   					let instance_type_decl = calling_class in
		   					 try
		   					 	(find_pub_mem mem instance_type_decl, TypIdent(instance_type_decl.cname))
		   					 with Failure(err) ->
		   					 	(find_priv_mem mem instance_type_decl, TypIdent(instance_type_decl.cname))
		   		    else   (* Invoking a member outside the class *)
		   		    	   (* Check that the object has been instantiatiated *)
		   		    	   let instance_type = type_of_identifier instance sym_tbl in
		   		    	   let instance_type_decl = find_class (ud_type_to_str instance_type) in
		   		    	   (* Check that the mem name is a valid public member in class *)
		   		    	   (find_pub_mem mem instance_type_decl, instance_type)
		   		in let (ty, _) = found_mem
		   		(* Return format *)
		   		(* (( typ, SClassMemAccess(string, string)), sym_tbl) *)
		   		in ((ty, SClassMemAccess(mem, instance)), sym_tbl)

		in

		(* let check_bool_expr e sym_tbl=
		  let ((t, e'), m) = check_expr e sym_tbl in
		  match t with
		  | Bool -> (t, e')
		  | _ -> raise (Failure ("expected Boolean expression"))
		in *)

		let rec check_stmt_list stmt_list locals symbols =
		  match stmt_list with
		    [] -> []
		  | s :: sl -> let (st, lc, sy) = check_stmt s locals symbols in st :: check_stmt_list sl lc sy
		(* Return a semantically-checked statement i.e. containing sexprs *)
		and check_stmt stmt locals (* list of locals *) symbols (* symbol map *) =
			match stmt with
		    Expr e -> let ((ty, str), tbl) = check_expr e symbols in (SExpr(ty, str), locals, symbols)
		   | Bind b ->
			     let (ty, name) = b in
			     let locals = locals @ [(ty, name)] in
			     let symbols = StringMap.add name ty symbols in
			     let () = check_binds "local" locals in
			     (SExpr(ty, SIdent(name)), locals, symbols)
		   | BindAssign (b, e) ->
		         let (ty, name) = b in
		         let locals = locals @ [(ty, name)] in
		         let symbols = StringMap.add name ty symbols in
		         let () = check_binds "local" locals in
		         let ((typ, str), tbl) = check_expr e symbols in
		         let err = "illegal assignment in expression" in
		         let _ = check_assign ty typ err in
		         (SBindAssign((ty, name), (typ, str)), locals, symbols)
		   | Assign(var, e) ->
		   	 let lt = type_of_identifier var symbols
		   	 and ((rt, e'), _) = check_expr e symbols in
		   	 let err = "illegal assignment in expression" in
		   	 let _ = check_assign lt rt err in
		 	 (SAssign(var, (rt, e')), locals, symbols)
		   | Return e ->
		     let ((t, e'), _) = check_expr e symbols in
		     if t = func.rtyp then (SReturn (t, e'), locals, symbols)
		     else raise (
		     	Failure ("return gives incorrect type")
		     )
		   (* TODO: Need to account for ClassMemRassn and Instance *)
		   | ClassMemRassn(mem, instance, expr) ->
		   	  let _ = "can't find public member " ^ mem in
		      let (found_mem, instance_type) =
		      	if instance = "DIS" then
		      	         (* Invoking a member within the class *)
		      			 (* Check both public and private members of class *)
		      			 let instance_type_decl = calling_class in
		      			 try
		      			 	(find_pub_mem mem instance_type_decl, TypIdent(instance_type_decl.cname))
		      			 with Failure(err) ->
		      				(find_priv_mem mem instance_type_decl, TypIdent(instance_type_decl.cname))
		      	else   (* Invoking a member outside the class *)
		   		       (* Check that the object has been instantiatiated *)
		   		       let instance_type = type_of_identifier instance symbols in
		   		       let instance_type_decl = find_class (ud_type_to_str instance_type) in
	   		    	   (* Check that the mem name is a valid public member in class *)
	   		    	   (find_pub_mem mem instance_type_decl, instance_type)
	   		   in
	   		   let ((rt, e'), _) = check_expr expr symbols in
	   		   let err = "illegal assignment in expression" in
	   		   let (lt, _) = found_mem in
 	   		   let _ = check_assign lt rt err in
	   		   (SClassMemRassn(mem, instance, (rt, e')), locals, symbols)
		   | Instance (ud_type, name) ->
		   		(* Make sure ud_type is a valid class *)
		   		let found_class = find_class (ud_type_to_str ud_type) in
		   		(* Make sure the class is not the current class
		   		i.e. we can't have an instance of the class within the class decl *)
		   		if found_class = calling_class then
		   			raise (Failure ("can't instantiate object of type " ^ found_class.cname ^
		   											"in declaration of " ^ found_class.cname))
		   	    else
	   	     	(* Add bind to local list and to symbol table *)
		     	let locals = locals @ [(ud_type, name)] in
		     	let symbols = StringMap.add name ud_type symbols in
		     	let () = check_binds "local" locals in
		     	(SInstance(ud_type, name), locals, symbols)
		in
		(* body of check_func *)
		{ srtyp = func.rtyp;
		  sfname = func.fname;
		  sformals = func.formals;
		  sbody = check_stmt_list func.body locals symbols
	    }

	in

	(* TODO: Check class constructor and destructor *)

	let check_cons (calling_class, cons)=

		let locals = [] in

		(* Raise an exception if the given rvalue type cannot be assigned to 
		   the given lvalue type *)
		let check_assign lvaluet rvaluet err = 
		  if lvaluet = rvaluet then lvaluet else raise (Failure err)
		in

		(* Build local symbol table of variables for this function *)
		let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
			StringMap.empty (globals @ calling_class.pubmembers
							 @ calling_class.privmembers)
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
				| (ty, name) when (* ty = type_of_identifier m symbols
								  &&  *) name = m -> true
				| _ -> false
			in 
			try List.find pred classd.pubmembers
			with Not_found -> raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
		in

		(* Return a private member from a class *)
		let find_priv_mem m classd =
			let pred = function
				(* Make sure both type and var name match *)
				| (ty, name) when (* ty = type_of_identifier m symbols
								  && *) name = m -> true
				| _ -> false
			in 
			try List.find pred classd.privmembers
			with Not_found -> raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
		in

		(* Return a semantically-checked expression (sexpr) , i.e. with a type *)
		let rec check_expr expr sym_tbl =
		   match expr with
		  | NullLit l -> ((Null, SNullLit l), sym_tbl)
		  | IntLit l -> ((Int, SIntLit l), sym_tbl)
		  | BoolLit l -> ((Bool, SBoolLit l), sym_tbl)
		  | StrLit  l -> ((String, SStrLit l), sym_tbl)
		  | Ident var -> ((type_of_identifier var sym_tbl, SIdent var), sym_tbl)
		  | Binop(e1, op, e2) ->
		    let ((t1, e1'), sym_tbl) = check_expr e1 sym_tbl in
		    let ((t2, e2'), sym_tbl) = check_expr e2 sym_tbl in
		    (* TODO: Make error more specific with pretty-printing functions *)
		    let err = "illegal binary operator " 
		  	in 
		  	(* All binary operators require operands of the same type *)
		  	if t1 = t2 then
		  	   (* Determine expression type based on operator and operator types *)
		  	   let t = match op with
		  	       Add | Sub | Mul | Div when t1 = Int -> Int
		  	     | Eq | Neq -> Bool
		  	     | Less when t1 = Int -> Bool
		  	     | And | Or when t1 = Bool -> Bool
		  	     | _ -> raise (Failure err)
		  	    in
		  	    ((t, SBinop((t1, e1'), op, (t2, e2'))), sym_tbl)
		  	else raise (Failure err)
		  | Unop(op, e1) ->
		  	let ((t1, e1'), m1) = check_expr e1 sym_tbl in
			(* Single unary operator must be not *)
			let t = match op with
			  | Not -> Bool
			 in
			 ((t, SUnop(op, (t1, e1'))), m1)
		  | Functcall(fname, args) ->
		  	let fd = find_func fname in
		  	let param_length = List.length fd.formals in
		  	if List.length args != param_length then
		  		raise (Failure ("expecting " ^ string_of_int param_length ^
		  						" arguments in function call"))
		  	else let check_call (ft, _) e =
		  		   let ((et, e'), sym_tbl) = check_expr e sym_tbl in
		  		   let err = "illegal argument found in function call"
		  		   in ((check_assign ft et err, e'), sym_tbl)
		       in 
		       let args' = List.map2 check_call fd.formals args in
		       let args'' = List.map (fun (a, b) -> a) args' in
		   	   ((fd.rtyp, SFunctcall(fname, args'')), sym_tbl)
		   	(* TODO: Need to account for ClassFunctcall, ClassMemAccess *)
		   | ClassFunctcall(fname, (instance, args)) ->
		   		let _ = "can't find public function " ^ fname in
		   		let fd = 
			   		if instance = "DIS" then
			   		        (* Calling a function within the class *)	
			   				(* Check both public and private functions of class *)
			   				try 
			   					find_pub_func fname calling_class 
			   				with Failure(err) -> 				
			   					find_priv_func fname calling_class
			   					 
			   		else   (* Calling a function outside the class *)
			   			   (* Check that the object has been instantiated within 
		   					the func scope *)
		   				   (* Check that fname is a valid public function in class *)
			   				find_pub_func fname (find_class (ud_type_to_str (type_of_identifier instance sym_tbl)))
			   		in
			   	let param_length = List.length fd.formals in
		   		  (* Check that number of arguments is correct *)
		   		  if List.length args != param_length then
		   		      raise (Failure ("expecting " ^ string_of_int param_length ^
		  						      " arguments in function call"))
		   		  else let check_call (ft, _) e =
			  		   let ((et, e'), sym_tbl) = check_expr e sym_tbl in
			  		   let err = "illegal argument found in function call"
			  		   in ((check_assign ft et err, e'), sym_tbl)
			       in 
			       (* Check that type of arguments is correct *)
			       let args' = List.map2 check_call fd.formals args in
			       let args'' = List.map (fun (a, b) -> a) args' in
			       (* Return format *)
			       (* ( ( typ , SClassFunctcall( string, (string, sexpr list ) ) ), sym_tbl) *)
				   ( ( fd.rtyp, SClassFunctcall (fname, (calling_class.cname, args''))), sym_tbl)
		   | ClassMemAccess(mem, instance) ->
		        let _ = "can't find public member " ^ mem in
		   		let (found_mem, instance_type) = 
		   			if instance = "DIS" then
		   			         (* Invoking a member within the class *)
		   					 (* Check both public and private members of class *)
		   					let instance_type_decl = calling_class in
		   					 try 
		   					 	(find_pub_mem mem instance_type_decl, TypIdent(instance_type_decl.cname))
		   					 with Failure(err) ->
		   					 	(find_priv_mem mem instance_type_decl, TypIdent(instance_type_decl.cname))
		   		    else   (* Invoking a member outside the class *)
		   		    	   (* Check that the object has been instantiatiated *)
		   		    	   let instance_type = type_of_identifier instance sym_tbl in
		   		    	   let instance_type_decl = find_class (ud_type_to_str instance_type) in
		   		    	   (* Check that the mem name is a valid public member in class *)
		   		    	   (find_pub_mem mem instance_type_decl, instance_type)
		   		in let (ty, _) = found_mem
		   		(* Return format *)
		   		(* (( typ, SClassMemAccess(string, string)), sym_tbl) *)
		   		in ((ty, SClassMemAccess(mem, instance)), sym_tbl)

		in

		(* let check_bool_expr e sym_tbl=
		  let ((t, e'), m) = check_expr e sym_tbl in
		  match t with
		  | Bool -> (t, e')
		  | _ -> raise (Failure ("expected Boolean expression"))
		in *)

		let rec check_stmt_list stmt_list locals symbols =
		  match stmt_list with
		    [] -> []
		  | s :: sl -> let (st, lc, sy) = check_stmt s locals symbols in st :: check_stmt_list sl lc sy
		(* Return a semantically-checked statement i.e. containing sexprs *)
		and check_stmt stmt locals (* list of locals *) symbols (* symbol map *) = 
			match stmt with
		    Expr e -> let ((ty, str), tbl) = check_expr e symbols in (SExpr(ty, str), locals, symbols)
		   | Bind b -> 
			     let (ty, name) = b in
			     let locals = locals @ [(ty, name)] in
			     let symbols = StringMap.add name ty symbols in
			     let () = check_binds "local" locals in
			     (SExpr(ty, SIdent(name)), locals, symbols)
		   | BindAssign (b, e) ->
		         let (ty, name) = b in
		         let locals = locals @ [(ty, name)] in
		         let symbols = StringMap.add name ty symbols in
		         let () = check_binds "local" locals in
		         let ((typ, str), tbl) = check_expr e symbols in
		         let err = "illegal assignment in expression" in
		         let _ = check_assign ty typ err in
		         (SBindAssign((ty, name), (typ, str)), locals, symbols) 
		   | Assign(var, e) ->
		   	 let lt = type_of_identifier var symbols
		   	 and ((rt, e'), _) = check_expr e symbols in
		   	 let err = "illegal assignment in expression" in
		   	 let _ = check_assign lt rt err in
		 	 (SAssign(var, (rt, e')), locals, symbols)
		   (* | Return e ->
		     let ((t, e'), _) = check_expr e symbols in 
		     if t = func.rtyp then (SReturn (t, e'), locals, symbols)
		     else raise (
		     	Failure ("return gives incorrect type")
		     ) *)
		   (* TODO: Need to account for ClassMemRassn and Instance *)
		   | ClassMemRassn(mem, instance, expr) ->
		   	  let _ = "can't find public member " ^ mem in
		      let (found_mem, instance_type) = 
		      	if instance = "DIS" then
		      	         (* Invoking a member within the class *)
		      			 (* Check both public and private members of class *)
		      			 let instance_type_decl = calling_class in
		      			 try
		      			 	(find_pub_mem mem instance_type_decl, TypIdent(instance_type_decl.cname))
		      			 with Failure(err) -> 
		      				(find_priv_mem mem instance_type_decl, TypIdent(instance_type_decl.cname))
		      	else   (* Invoking a member outside the class *)
		   		       (* Check that the object has been instantiatiated *)
		   		       let instance_type = type_of_identifier instance symbols in
		   		       let instance_type_decl = find_class (ud_type_to_str instance_type) in
	   		    	   (* Check that the mem name is a valid public member in class *)
	   		    	   (find_pub_mem mem instance_type_decl, instance_type)
	   		   in 
	   		   let ((rt, e'), _) = check_expr expr symbols in
	   		   let err = "illegal assignment in expression" in
	   		   let (lt, _) = found_mem in 
 	   		   let _ = check_assign lt rt err in
	   		   (SClassMemRassn(mem, instance, (rt, e')), locals, symbols)
		   | Instance (ud_type, name) ->
		   		(* Make sure ud_type is a valid class *)
		   		let found_class = find_class (ud_type_to_str ud_type) in
		   		(* Make sure the class is not the current class
		   		i.e. we can't have an instance of the class within the class decl *)
		   		if found_class = calling_class then
		   			raise (Failure ("can't instantiate object of type " ^ found_class.cname ^
		   											"in declaration of " ^ found_class.cname))
		   	    else
	   	     	(* Add bind to local list and to symbol table *)
		     	let locals = locals @ [(ud_type, name)] in
		     	let symbols = StringMap.add name ud_type symbols in
		     	let () = check_binds "local" locals in
		     	(SInstance(ud_type, name), locals, symbols)
		in 
		check_stmt_list cons locals symbols

	in

	let check_des (calling_class, des) = 

		let locals = [] in

		(* Raise an exception if the given rvalue type cannot be assigned to 
		   the given lvalue type *)
		let check_assign lvaluet rvaluet err = 
		  if lvaluet = rvaluet then lvaluet else raise (Failure err)
		in

		(* Build local symbol table of variables for this function *)
		let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
			StringMap.empty (globals @ calling_class.pubmembers
							 @ calling_class.privmembers)
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
				| (ty, name) when (* ty = type_of_identifier m symbols
								  &&  *) name = m -> true
				| _ -> false
			in 
			try List.find pred classd.pubmembers
			with Not_found -> raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
		in

		(* Return a private member from a class *)
		let find_priv_mem m classd =
			let pred = function
				(* Make sure both type and var name match *)
				| (ty, name) when (* ty = type_of_identifier m symbols
								  && *) name = m -> true
				| _ -> false
			in 
			try List.find pred classd.privmembers
			with Not_found -> raise (Failure ("no such member " ^ m ^ " in class " ^ classd.cname))
		in

		(* Return a semantically-checked expression (sexpr) , i.e. with a type *)
		let rec check_expr expr sym_tbl =
		   match expr with
		  | NullLit l -> ((Null, SNullLit l), sym_tbl)
		  | IntLit l -> ((Int, SIntLit l), sym_tbl)
		  | BoolLit l -> ((Bool, SBoolLit l), sym_tbl)
		  | StrLit  l -> ((String, SStrLit l), sym_tbl)
		  | Ident var -> ((type_of_identifier var sym_tbl, SIdent var), sym_tbl)
		  | Binop(e1, op, e2) ->
		    let ((t1, e1'), sym_tbl) = check_expr e1 sym_tbl in
		    let ((t2, e2'), sym_tbl) = check_expr e2 sym_tbl in
		    (* TODO: Make error more specific with pretty-printing functions *)
		    let err = "illegal binary operator " 
		  	in 
		  	(* All binary operators require operands of the same type *)
		  	if t1 = t2 then
		  	   (* Determine expression type based on operator and operator types *)
		  	   let t = match op with
		  	       Add | Sub | Mul | Div when t1 = Int -> Int
		  	     | Eq | Neq -> Bool
		  	     | Less when t1 = Int -> Bool
		  	     | And | Or when t1 = Bool -> Bool
		  	     | _ -> raise (Failure err)
		  	    in
		  	    ((t, SBinop((t1, e1'), op, (t2, e2'))), sym_tbl)
		  	else raise (Failure err)
		  | Unop(op, e1) ->
		  	let ((t1, e1'), m1) = check_expr e1 sym_tbl in
			(* Single unary operator must be not *)
			let t = match op with
			  | Not -> Bool
			 in
			 ((t, SUnop(op, (t1, e1'))), m1)
		  | Functcall(fname, args) ->
		  	let fd = find_func fname in
		  	let param_length = List.length fd.formals in
		  	if List.length args != param_length then
		  		raise (Failure ("expecting " ^ string_of_int param_length ^
		  						" arguments in function call"))
		  	else let check_call (ft, _) e =
		  		   let ((et, e'), sym_tbl) = check_expr e sym_tbl in
		  		   let err = "illegal argument found in function call"
		  		   in ((check_assign ft et err, e'), sym_tbl)
		       in 
		       let args' = List.map2 check_call fd.formals args in
		       let args'' = List.map (fun (a, b) -> a) args' in
		   	   ((fd.rtyp, SFunctcall(fname, args'')), sym_tbl)
		   	(* TODO: Need to account for ClassFunctcall, ClassMemAccess *)
		   | ClassFunctcall(fname, (instance, args)) ->
		   		let _ = "can't find public function " ^ fname in
		   		let fd = 
			   		if instance = "DIS" then
			   		        (* Calling a function within the class *)	
			   				(* Check both public and private functions of class *)
			   				try 
			   					find_pub_func fname calling_class 
			   				with Failure(err) -> 				
			   					find_priv_func fname calling_class
			   					 
			   		else   (* Calling a function outside the class *)
			   			   (* Check that the object has been instantiated within 
		   					the func scope *)
		   				   (* Check that fname is a valid public function in class *)
			   				find_pub_func fname (find_class (ud_type_to_str (type_of_identifier instance sym_tbl)))
			   		in
			   	let param_length = List.length fd.formals in
		   		  (* Check that number of arguments is correct *)
		   		  if List.length args != param_length then
		   		      raise (Failure ("expecting " ^ string_of_int param_length ^
		  						      " arguments in function call"))
		   		  else let check_call (ft, _) e =
			  		   let ((et, e'), sym_tbl) = check_expr e sym_tbl in
			  		   let err = "illegal argument found in function call"
			  		   in ((check_assign ft et err, e'), sym_tbl)
			       in 
			       (* Check that type of arguments is correct *)
			       let args' = List.map2 check_call fd.formals args in
			       let args'' = List.map (fun (a, b) -> a) args' in
			       (* Return format *)
			       (* ( ( typ , SClassFunctcall( string, (string, sexpr list ) ) ), sym_tbl) *)
				   ( ( fd.rtyp, SClassFunctcall (fname, (calling_class.cname, args''))), sym_tbl)
		   | ClassMemAccess(mem, instance) ->
		        let _ = "can't find public member " ^ mem in
		   		let (found_mem, instance_type) = 
		   			if instance = "DIS" then
		   			         (* Invoking a member within the class *)
		   					 (* Check both public and private members of class *)
		   					let instance_type_decl = calling_class in
		   					 try 
		   					 	(find_pub_mem mem instance_type_decl, TypIdent(instance_type_decl.cname))
		   					 with Failure(err) ->
		   					 	(find_priv_mem mem instance_type_decl, TypIdent(instance_type_decl.cname))
		   		    else   (* Invoking a member outside the class *)
		   		    	   (* Check that the object has been instantiatiated *)
		   		    	   let instance_type = type_of_identifier instance sym_tbl in
		   		    	   let instance_type_decl = find_class (ud_type_to_str instance_type) in
		   		    	   (* Check that the mem name is a valid public member in class *)
		   		    	   (find_pub_mem mem instance_type_decl, instance_type)
		   		in let (ty, _) = found_mem
		   		(* Return format *)
		   		(* (( typ, SClassMemAccess(string, string)), sym_tbl) *)
		   		in ((ty, SClassMemAccess(mem, instance)), sym_tbl)

		in

		(* let check_bool_expr e sym_tbl=
		  let ((t, e'), m) = check_expr e sym_tbl in
		  match t with
		  | Bool -> (t, e')
		  | _ -> raise (Failure ("expected Boolean expression"))
		in *)

		let rec check_stmt_list stmt_list locals symbols =
		  match stmt_list with
		    [] -> []
		  | s :: sl -> let (st, lc, sy) = check_stmt s locals symbols in st :: check_stmt_list sl lc sy
		(* Return a semantically-checked statement i.e. containing sexprs *)
		and check_stmt stmt locals (* list of locals *) symbols (* symbol map *) = 
			match stmt with
		    Expr e -> let ((ty, str), tbl) = check_expr e symbols in (SExpr(ty, str), locals, symbols)
		   | Bind b -> 
			     let (ty, name) = b in
			     let locals = locals @ [(ty, name)] in
			     let symbols = StringMap.add name ty symbols in
			     let () = check_binds "local" locals in
			     (SExpr(ty, SIdent(name)), locals, symbols)
		   | BindAssign (b, e) ->
		         let (ty, name) = b in
		         let locals = locals @ [(ty, name)] in
		         let symbols = StringMap.add name ty symbols in
		         let () = check_binds "local" locals in
		         let ((typ, str), tbl) = check_expr e symbols in
		         let err = "illegal assignment in expression" in
		         let _ = check_assign ty typ err in
		         (SBindAssign((ty, name), (typ, str)), locals, symbols) 
		   | Assign(var, e) ->
		   	 let lt = type_of_identifier var symbols
		   	 and ((rt, e'), _) = check_expr e symbols in
		   	 let err = "illegal assignment in expression" in
		   	 let _ = check_assign lt rt err in
		 	 (SAssign(var, (rt, e')), locals, symbols)
		   (* | Return e ->
		     let ((t, e'), _) = check_expr e symbols in 
		     if t = func.rtyp then (SReturn (t, e'), locals, symbols)
		     else raise (
		     	Failure ("return gives incorrect type")
		     ) *)
		   (* TODO: Need to account for ClassMemRassn and Instance *)
		   | ClassMemRassn(mem, instance, expr) ->
		   	  let _ = "can't find public member " ^ mem in
		      let (found_mem, instance_type) = 
		      	if instance = "DIS" then
		      	         (* Invoking a member within the class *)
		      			 (* Check both public and private members of class *)
		      			 let instance_type_decl = calling_class in
		      			 try
		      			 	(find_pub_mem mem instance_type_decl, TypIdent(instance_type_decl.cname))
		      			 with Failure(err) -> 
		      				(find_priv_mem mem instance_type_decl, TypIdent(instance_type_decl.cname))
		      	else   (* Invoking a member outside the class *)
		   		       (* Check that the object has been instantiatiated *)
		   		       let instance_type = type_of_identifier instance symbols in
		   		       let instance_type_decl = find_class (ud_type_to_str instance_type) in
	   		    	   (* Check that the mem name is a valid public member in class *)
	   		    	   (find_pub_mem mem instance_type_decl, instance_type)
	   		   in 
	   		   let ((rt, e'), _) = check_expr expr symbols in
	   		   let err = "illegal assignment in expression" in
	   		   let (lt, _) = found_mem in 
 	   		   let _ = check_assign lt rt err in
	   		   (SClassMemRassn(mem, instance, (rt, e')), locals, symbols)
		   | Instance (ud_type, name) ->
		   		(* Make sure ud_type is a valid class *)
		   		let found_class = find_class (ud_type_to_str ud_type) in
		   		(* Make sure the class is not the current class
		   		i.e. we can't have an instance of the class within the class decl *)
		   		if found_class = calling_class then
		   			raise (Failure ("can't instantiate object of type " ^ found_class.cname ^
		   											"in declaration of " ^ found_class.cname))
		   	    else
	   	     	(* Add bind to local list and to symbol table *)
		     	let locals = locals @ [(ud_type, name)] in
		     	let symbols = StringMap.add name ud_type symbols in
		     	let () = check_binds "local" locals in
		     	(SInstance(ud_type, name), locals, symbols)
		in 
		check_stmt_list des locals symbols

	in

	(* TODO: Check a class *)
	let check_class class_decl =

		check_binds "public member" class_decl.pubmembers;
		check_binds "private member" class_decl.privmembers;

		let class_pubfunc_binds = List.map (fun f -> (TypIdent(class_decl.cname), f.fname)) class_decl.pubfuncs in
		let class_privfunc_binds = List.map (fun f -> (TypIdent(class_decl.cname), f.fname)) class_decl.privfuncs in

		check_binds "public function" class_pubfunc_binds;
		check_binds "private function" class_privfunc_binds;

		let class_pubfuncs = List.map (fun f -> (class_decl, f)) class_decl.pubfuncs in
		let class_privfuncs = List.map (fun f -> (class_decl, f)) class_decl.privfuncs in
		let class_cons = List.map (fun f -> (class_decl, f)) class_decl.cons in
		let class_des = List.map (fun f -> (class_decl, f)) class_decl.des in

		{
		  scname = class_decl.cname;
		  spubmembers = class_decl.pubmembers;
		  sprivmembers = class_decl.privmembers;
		  spubfuncs = List.map check_class_func class_pubfuncs;
		  sprivfuncs = List.map check_class_func class_privfuncs;
		  scons = List.map check_cons class_cons;
		  sdes = List.map check_des class_des
		}
    in
    {
        sclasses=List.map check_class classes;
        sfunctions=List.map check_func functions;
        sglobals=globals
    }
