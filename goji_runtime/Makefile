SOURCES=goji_internal.ml

.PHONY: clean doc depends install uninstall

all: depends goji_runtime.cma

goji_runtime.cma: $(patsubst %.ml, %.cmo, $(SOURCES))
	ocamlfind ocamlc -a -g -package dynlink,str,pprint,unix $^ -o goji_runtime.cma

%.cmx: %.ml
	ocamlfind ocamlopt -package pprint -c $<

%.cmo: %.ml
	ocamlfind ocamlc -package pprint -c $<

doc: $(SOURCES)
	if [ ! -e doc ] ; then mkdir doc ; fi
	ocamldoc -html -d doc $(SOURCES)

clean:
	rm -f *.cm* *~ *.o *goji_runtime.*
	rm -rf doc


install: all
	ocamlfind install goji_runtime META goji_runtime.* $(patsubst %.ml, %.cmi, $(SOURCES))

uninstall: all
	ocamlfind remove goji_runtime

-include .depends

depends:
	ocamldep *.ml > .depends
