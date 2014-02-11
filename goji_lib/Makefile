DOC_SOURCES= \
  goji_ast.ml goji_dsl.ml \
  goji_license.ml goji_grab.ml goji_registry.ml \
  goji.ml

SOURCES= \
  goji_config.ml goji_messages.ml goji_syntax.ml \
  $(DOC_SOURCES)

.PHONY: clean doc depends install uninstall

all: depends goji_lib.cma goji_lib.cmxa

goji_lib.cma: $(patsubst %.ml, %.cmo, $(SOURCES))
	ocamlfind ocamlc -a -g -package dynlink,str $^ -o goji_lib.cma

goji_lib.cmxa: $(patsubst %.ml, %.cmx, $(SOURCES))
	ocamlfind ocamlopt -a -g -package dynlink,str $^ -o goji_lib.cmxa

%.cmx: %.ml
	ocamlfind ocamlopt -g -package pprint -c $<

%.cmo: %.ml
	ocamlfind ocamlc -g -package pprint -c $<

%.cmi: %.mli
	ocamlfind ocamlc -package pprint -c $<

doc: $(SOURCES)
	if [ ! -e doc ] ; then mkdir doc ; fi
	ocamldoc -html -d doc $(DOC_SOURCES) *.mli

clean:
	rm -f *.cm* *~ *.o *goji_lib.*
	rm -rf doc

install: all
	ocamlfind install goji_lib META goji_lib.* $(patsubst %.ml, %.cmi, $(SOURCES))

uninstall: all
	ocamlfind remove goji_lib

-include .depends

depends:
	ocamldep *.ml > .depends
