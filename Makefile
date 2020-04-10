build:
	ocamlbuild -Is interpreter,interpreter/builtin feline.native

clean:
	ocamlbuild -Is interpreter,interpreter/builtin -clean
	rm -f parser.ml
	rm -f parser.mli
	rm -f parser.output

yacc:
	ocamlyacc -v parser.mly

.PHONY: all clean
