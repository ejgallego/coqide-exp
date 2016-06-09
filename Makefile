.PHONY: clean all ide-native ide-byte

OCB=ocamlbuild
OCB_OPT=-use-ocamlfind -j 4

all: ide-native

ide-native:
	$(OCB) $(OCB_OPT) ide/coqide_main.native

ide-byte:
	$(OCB) $(OCB_OPT) ide/coqide_main.byte

clean:
	$(OCB) $(OCB_OPT) -clean
