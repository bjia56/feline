all:
	ocamlbuild -Is interpreter,interpreter/builtin feline.native

clean:
	ocamlbuild -Is interpreter,interpreter/builtin -clean

.PHONY: all clean
