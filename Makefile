build:
	ocamlbuild -pkgs llvm -Is tests,builtins feline.native

clean:
	ocamlbuild -pkgs llvm -Is tests,builtins -clean
	rm -f parser.ml
	rm -f parser.mli
	rm -f parser.output

yacc:
	ocamlyacc -v parser.mly

.PHONY: all clean
