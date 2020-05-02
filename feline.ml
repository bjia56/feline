open Ast

let ast_of_chan chan =
  let lexbuf = Lexing.from_channel chan in
  try Parser.module_decl Scanner.token lexbuf
  with Parsing.Parse_error ->
    raise (Utils.SyntaxError (Utils.syntax_error_string lexbuf))

let ast_of_file file_name =
  let () = Utils.update_current_file file_name in
  let file_chan = open_in file_name in
  let ast = ast_of_chan file_chan in
  let () = close_in file_chan in
  ast

let empty_string_list : string list = []

module StringMap = Map.Make (String)

exception InvalidFelineFileName of string

let rec split_string (c : char) (s : string) : string list =
  if s = "" then []
  else
    let len = String.length s in
    if c = s.[0] then "" :: split_string c (String.sub s 1 (len - 1))
    else
      let tail_list = split_string c (String.sub s 1 (len - 1)) in
      match tail_list with
      | [] -> [ Char.escaped s.[0] ]
      | hd :: tl -> (Char.escaped s.[0] ^ hd) :: tl

let validate_and_trim_file_extension (file : string) : string =
  let components = split_string '.' file in
  if 2 <> List.length components then raise (InvalidFelineFileName file)
  else
    let first = List.nth components 0 in
    let second = List.nth components 1 in
    if second <> "cat" then raise (InvalidFelineFileName file) else first

let get_module_name (file : string) : string =
  let trimmed = validate_and_trim_file_extension file in
  let components = split_string '/' trimmed in
  List.hd (List.rev components)

let rec asts_of_file_list (files : string list) : Ast.module_decl StringMap.t =
  match files with
  | [] -> StringMap.empty
  | hd :: tl ->
      let tl_result = asts_of_file_list tl in
      let module_name = get_module_name hd in
      if StringMap.mem module_name tl_result then
        raise (Failure ("duplicate module " ^ module_name))
      else StringMap.add module_name (ast_of_file hd) tl_result

let print_parsed_modules (asts : Ast.module_decl StringMap.t) =
  let is_first = ref true in
  let print_binding k v =
    if not !is_first then
      let () = print_string ", " in
      print_string k
    else
      let () = is_first := false in
      print_string k
  in
  let () = print_string "Found modules: " in
  let () = StringMap.iter print_binding asts in
  print_endline ""

let ir_file_of_llmodule (use_tmp : bool) (name : string) (lm : Llvm.llmodule) =
  let fname = if use_tmp then Filename.temp_file name ".ir" else name ^ ".ir" in
  let () = Llvm.print_module fname lm in
  fname

let obj_file_of_ir_file (ir_file : string) =
  let out = ir_file ^ ".o" in
  let cmd = "llc -filetype=obj " ^ ir_file ^ " -o " ^ out in
  let () = print_endline cmd in
  let ret = Sys.command cmd in
  if ret <> 0 then raise (Failure ("llc exited with code " ^ string_of_int ret))
  else out

let gcc_objects (output : string) (obj_files : string list) =
  let obj_arg_list =
    List.fold_left
      (fun acc obj_file -> acc ^ " " ^ obj_file)
      (List.hd obj_files) (List.tl obj_files)
  in
  let cmd =
    "gcc -o " ^ output ^ " " ^ obj_arg_list ^ " "
    ^ BuiltinsLoader.library_src_dir ^ "/*.c"
  in
  let () = print_endline cmd in
  let ret = Sys.command cmd in
  if ret <> 0 then raise (Failure ("gcc exited with code " ^ string_of_int ret))
  else ()

let _ =
  let files = ref empty_string_list in
  let testcases = ref false in
  let output = ref "a.out" in
  let ir_only = ref false in
  let speclist =
    [
      ( "-file",
        Arg.String (fun x -> files := !files @ [ x ]),
        "Input file to compile" );
      ("-test", Arg.Set testcases, "Run compiler test cases");
      ("-out", Arg.Set_string output, "File name of compiled binary");
      ("-ir", Arg.Set ir_only, "Generate IR only");
    ]
  in
  let usage =
    "Compiler for the FELINE programming language. Options available:"
  in
  let () = Arg.parse speclist (fun x -> ()) usage in

  if !testcases then ParserTests.run_tests ()
  else if List.length !files = 0 then print_endline "No input files, exiting"
  else
    let () =
      print_endline
        ("Compiling " ^ string_of_int (List.length !files) ^ " files...")
    in
    let ir_files = ref StringMap.empty in
    let obj_files = ref StringMap.empty in
    let _ =
      try
        let asts = asts_of_file_list !files in
        let () = print_parsed_modules asts in
        let sasts =
          StringMap.mapi
            (Semant.check_module
               (BuiltinsLoader.with_builtins asts)
               StringMap.empty)
            asts
        in
        let llmodules = StringMap.mapi Irgen.translate sasts in
        if !ir_only then
          let _ = StringMap.mapi (ir_file_of_llmodule false) llmodules in
          ()
        else
          let () =
            ir_files := StringMap.mapi (ir_file_of_llmodule true) llmodules
          in
          let () = obj_files := StringMap.map obj_file_of_ir_file !ir_files in

          let () =
            gcc_objects !output
              (List.map (fun (_, v) -> v) (StringMap.bindings !obj_files))
          in

          let _ = StringMap.map Llvm.dispose_module llmodules in
          print_endline "Compilation complete"
      with
      | Utils.SyntaxError e -> print_endline e
      | Failure e -> print_endline ("compilation error: " ^ e)
    in
    let _ = StringMap.map Sys.remove !ir_files in
    let _ = StringMap.map Sys.remove !obj_files in
    ()
