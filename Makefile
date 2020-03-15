all:
	ocamlbuild -Is builtin feline.native

clean:
	ocamlbuild -Is builtin -clean

.PHONY: all clean
