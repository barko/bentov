NAME = bentov
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

install:
	@ocamlfind install $(NAME) lib/META _build/lib/bentov.cma _build/lib/bentov.cmxa _build/lib/bentov.cmi lib/bentov.mli

uninstall:
	@ocamlfind remove $(NAME)

all: lib tools test

.PHONY: all lib tools test install uninstall
