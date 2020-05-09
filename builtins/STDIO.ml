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
        {
          rtyp = Bool;
          fname = "FIN";
          formals = [];
          body = [ Return (BoolLit true) ];
        };
      ];
    globals = [];
  }

let module_signature = ("STDIO", ast)
