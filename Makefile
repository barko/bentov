OCB_FLAGS = -use-ocamlfind -I lib -I src
OCB = ocamlbuild $(OCB_FLAGS)

lib:
	$(OCB) bentov.cma
	$(OCB) bentov.cmxa

tools:
	$(OCB) bt.native
	$(OCB) bt.byte

test:
	$(OCB) test.native
	$(OCB) test.byte

all: lib tools test

.PHONY: all lib tools test
