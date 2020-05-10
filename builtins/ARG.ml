open Ast

let ast =
  {
    imports = [];
    classes = [];
    functions =
      [
        {
          rtyp = TypIdent "STRIN";
          fname = "NEXTARG";
          formals = [];
          body = [ Return (StrLit "") ];
        };
      ];
    globals = [];
  }

let module_signature = ("ARG", ast)

