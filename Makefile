all:
	ocamlbuild feline.native

clean:
	ocamlbuild -clean

.PHONY: all clean
