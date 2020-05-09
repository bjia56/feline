open Ast

let ast =
  {
    imports = [];
    classes =
      [
        {
          cname = "FYL";
          pubmembers = [];
          privmembers = [ (PtrAsInt, "path"); (PtrAsInt, "handle"); (Bool, "eof") ];
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
              {
                rtyp = Int;
                fname = "SYZ";
                formals = [];
                body = [ Return (IntLit 0) ];
              };
              {
                rtyp = Bool;
                fname = "FIN";
                formals = [];
                body = [ Return (BoolLit true) ];
              }
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
          body = [ Return (NewInstance "FYL") ];
        };
        {
          rtyp = Bool;
          fname = "IZFYL";
          formals = [ (TypIdent "STRIN", "path") ];
          body = [ Return (BoolLit true) ];
        };
        {
          rtyp = Bool;
          fname = "IZDYR";
          formals = [ (TypIdent "STRIN", "path") ];
          body = [ Return (BoolLit true) ];
         };
      ];
    globals = [];
  }

let module_signature = ("FIO", ast)
