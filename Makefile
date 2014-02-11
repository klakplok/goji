PREFIX ?= $(shell ocamlfind printconf path | sed s_/lib*__)
BINDIR ?= $(PREFIX)/bin

.PHONY: clean depends goji_lib goji_runtime tgz doc

all: goji_runtime goji_lib goji doc

OCAMLFIND=OCAMLPATH=. ocamlfind
PACKAGES=dynlink,str,unix,pprint,cmdliner,goji_lib,findlib

CMXS= goji_pprint.cmx goji_emit_struct.cmx goji_emit_adt.cmx \
      goji_check_and_fix.cmx goji_generate.cmx goji_jslink.cmx goji_main.cmx

goji: $(CMXS) goji_lib
	$(OCAMLFIND) ocamlopt -g -package $(PACKAGES) $(CMXS) -linkpkg -linkall -o $@

%.cmx: %.ml goji_lib
	$(OCAMLFIND) ocamlopt -g -package $(PACKAGES) -c $<

goji_lib:
	cd goji_lib && make

goji_runtime:
	cd goji_runtime && make

doc:
	cd goji_lib && make doc
	cd goji_runtime && make doc

clean:
	rm -f *.cm* */*.cm* *~ */*~ *.o */*.o *.so goji
	cd goji_runtime && make clean
	cd goji_lib && make clean

install: all
	cd goji_runtime && make install
	cd goji_lib && make install
	install goji $(BINDIR)/goji

uninstall:
	cd goji_runtime && make uninstall
	cd goji_lib && make uninstall
	rm $(BINDIR)/goji

ARCHIVE=goji-$(shell date "+%Y%m%d-%H%M").tgz

tgz: clean
	cd .. && tar czf $(ARCHIVE) goji

-include .depends

depends:
	ocamldep *.ml > .depends
