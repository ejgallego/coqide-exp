.PHONY: clean all ide-native ide-byte

OCB=ocamlbuild
OCB_OPT=-use-ocamlfind -j 4

all: ide-native

ide-native:
	rm -f coqidetop.cmxs
	$(OCB) $(OCB_OPT) ide_top/coqidetop.cmxs ide/coqide_main.native
	cp -a _build/ide_top/coqidetop.cmxs .

ide-byte:
	rm -f coqidetop.cma
	$(OCB) $(OCB_OPT) ide_top/coqidetop.cma ide/coqide_main.byte
	cp -a _build/ide_top/coqidetop.cma

clean:
	$(OCB) $(OCB_OPT) -clean
	rm -f coqidetop.cma coqidetop.cmxs
