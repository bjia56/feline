open Ast

let ast =
  {
    imports = [];
    classes =
      [
        {
          cname = "STRIN";
          pubmembers = [];
          privmembers = [ (PtrAsInt, "contents"); (Int, "length") ];
          pubfuncs = [];
          privfuncs = [];
          cons = [ [] ];
          des = [ [] ];
        };
      ];
    functions = [];
    globals = [];
  }

let module_signature = ("globalBuiltins", ast)
