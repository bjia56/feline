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
          pubfuncs = [
              {
                  rtyp = TypIdent "STRIN";
                  fname = "CONCAT";
                  formals = [ (TypIdent "STRIN", "y") ];
                  body = [ Return (NewInstance "STRIN") ];
              }
          ];
          privfuncs = [];
          cons = [ [] ];
          des = [ [] ];
        };
      ];
    functions = [
        {
            rtyp = TypIdent "STRIN";
            fname = "ITOA";
            formals = [ (Int, "x") ];
            body = [ Return (NewInstance "STRIN") ];
        }
    ];
    globals = [];
  }

let module_signature = ("globalBuiltins", ast)
