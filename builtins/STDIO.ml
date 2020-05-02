open Ast

let ast =
  {
    imports = [];
    classes = [];
    functions =
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
    globals = [];
  }

let module_signature = ("STDIO", ast)
