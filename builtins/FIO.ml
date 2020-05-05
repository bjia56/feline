open Ast

let ast =
  {
    imports = [];
    classes =
      [
        {
          cname = "FYL";
          pubmembers = [];
          privmembers = [ (PtrAsInt, "handle"); (Int, "isOpen") ];
          pubfuncs =
            [
              {
                rtyp = Void;
                fname = "MEOW";
                formals = [ (TypIdent "STRIN", "x") ];
                body = [];
              };
              {
                rtyp = TypIdent "STRIN";
                fname = "NOM";
                formals = [];
                body = [ Return (StrLit "") ];
              };
            ];
          privfuncs = [];
          cons = [];
          des = [];
        };
      ];
    functions =
      [
        {
          rtyp = TypIdent "FYL";
          fname = "OPEN";
          formals = [ (TypIdent "STRIN", "path") ];
          body = [];
        };
      ];
    globals = [];
  }

let module_signature = ("FIO", ast)
