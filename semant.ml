(* Semantic checking for Feline *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Pretty printing functions *)
let string_of_typ = function
      Void -> "void"
    | Null -> "null"
    | Int -> "int"
    | String -> "string"
    | Exception of string -> "exception"
    | TypIdent of string -> "custom type"

(* Semantic checking of AST. Returns SAST if successful, otherwise
   throws an exception.

   Check each global variable, then check classes, then check each function *)

let check (globals, classes, functions) =

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

    (* TODO: Add check function for classes *)

    (* Collect function declarations for built-in functions: no bodies *)
    let built_in_decls = 
      StringMap.add "MEOW" {
      	rtype = Int;
      	fname = "MEOW";
      	formals = [(Int, "x")];
      	body = [] } StringMap.empty
      }

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
	let temp_map = List.fold_left add_func built_in_decls functions
	in

	(* TODO: Need to check that this correctly adds function names from classes *)
	let function_decls = List.fold_left add_func () classes.functions

	(* Return a function from our symbol table *)
	let find_func s = 
		try StringMap.find s function_decls
		with Not_found -> raise (Failure ("unrecognized function " ^ s))
	in

	let _ = find_func "main" in

	let check_func func = 
		(* Make sure no formals or locals are void or duplicates *)
		check_binds "formal" func.formals;

		(* TODO: need to find out how to obtain local variables from function body *)
		(* check_binds "local" (* add locals here *) *)

		(* Raise an exception if the given rvalue type cannot be assigned to 
		   the given lvalue type *)
		let check_assign lvaluet rvaluet make_err = 
		  if lvaluet = rvaluet then lvaluet else raise (Failure err)
		in

		(* Build local symbol table of variables for this function *)
		let symbols = List.fold_left (fun m (ty, name) -> StringMapp.add name ty m)
			StringMap.empty (globals @ func.formals @ (* add locals here *))

		(* Return a variable from our local symbol table *)
		let type_of_identifier s =
		  try StringMap.find s symbols
		  with Not_found -> raise (Failure ("undeclared identifier" ^ s))
		in

		(* Return a semantically-checked expression (sexpr) , i.e. with a type *)
		let rec check_expr = function
		    Literal l -> (Int, SIntLit l)
		  | BoolLit l -> (Bool, SBoolLit l)
		  | StrLit  l -> (String, SStrLit l)
		  | Ident var -> (type_of_identifier var, SIdent var)
		  | Binop(e1, op, e2) as e ->
		    let (t1, e1') = check_expr e1
		    and (t2, e2') = check_expr e2 in
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
		  	    (t, SBinop((t1, e1'), op, (t2, e2')))
		  	else raise (Failure err)
		  | Unop(op, e1) as e ->
		  	let (t1, e1') = check_expr e1 in
		  	 (* TODO: Make error more specific with pretty-printing functions *)
		    let err = "illegal unary operator " 
			in
			(* Single unary operator must be not *)
			let t = match op with
			  | Not -> Bool
			  | _ -> raise (Failure err)
			 in
			 (t, SUnop(op, (t1, e1')))
		  | Call(fname, args) as call ->
		  	let fd = find_func fname in
		  	let param_length = List.length fd.formals in
		  	if List.length args != param_length then
		  		raise (Failure ("expecting " ^ string_of_int param_length ^
		  						" arguments in function call"))
		  	else let check_call (ft, _) e =
		  		   let (et, e') = check_expr e in
		  		   let err = "illegal argument found in function call"
		  		   in (check_assign ft et err, e')
		       in 
		       let args' = List.map2 check_call fd.formals args
		   	   in (fd.rtyp, SCall(fname, args'))
		in

		let check_bool_expr e =
		  let (t, e') = check_expr e in
		  match t with
		  | Bool -> (t, e')
		  | _ -> raise (Failure ("expected Boolean expression"))
		in

		let rec check_stmt_list = function
		    [] -> []
		  | s :: sl -> check_stmt s :: check_stmt_list sl
		(* Return a semantically-checked statement i.e. containing sexprs *)
		and check_stmt = function
		    Expr e -> SExpr (check_expr e)
		    (* TODO: add cases for Bind and BindAssign *)
		   | Assign(var, e) as ex ->
		   	 let lt = type_of_identifier var
		   	 and (rt, e') = check_expr e in
		   	 let err = "illegal assignment in expression"
		   	in 
		   	let x = (check_assign lt rt err, SAssign(var, (rt, e')))
		   	in (var, x)
		   | Return e ->
		     let (t, e') = check_expr e in 
		     if t = func.rtyp then SReturn (t, e')
		     else raise (
		     	Failure ("return gives incorrect type")
		     ) 
		in (* body of check_func *)
		{ srtyp = func.rtyp;
		  sfname = func.fname;
		  sformals = func.formals;
		  sbody = check_stmt_list func.body}
	    }
	in
	(globals, List.map check_func functions)














