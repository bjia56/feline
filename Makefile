build:
	ocamlbuild -pkgs llvm -Is tests feline.native

clean:
	ocamlbuild -pkgs llvm -Is tests -clean
	rm -f parser.ml
	rm -f parser.mli
	rm -f parser.output

yacc:
	ocamlyacc -v parser.mly

.PHONY: all clean
