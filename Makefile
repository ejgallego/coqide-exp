.PHONY: clean all ide

OCB=ocamlbuild
OCB_OPT=-use-ocamlfind -j 4

all: ide

TARGET=byte

ide:
	rm -f coqidetop.cma coqidetop.cmxs
	OCAMLFIND_IGNORE_DUPS_IN=/home/egallego/.opam/4.03.0/lib/ocaml/compiler-libs/ \
	$(OCB) $(OCB_OPT) ide_top/coqidetop.cma
	$(OCB) $(OCB_OPT) ide_top/coqidetop.cmxs
	$(OCB) $(OCB_OPT) ide/coqide_main.byte
	$(OCB) $(OCB_OPT) ide/coqide_main.native
	cp -a _build/ide_top/coqidetop.cma _build/ide_top/coqidetop.cmxs .
clean:
	$(OCB) $(OCB_OPT) -clean
	rm -f coqidetop.cma coqidetop.cmxs
