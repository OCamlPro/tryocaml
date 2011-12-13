
include ../../Makefile.conf
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLFIND = ocamlfind 
EXEC=tutorial
LIBS=unix.cma str.cma
SOURCES=utils.ml \
	tutorial.ml

.SUFFIXES: .ml .mli .cmo .cmi .cmx

all: depend $(EXEC)

opt : depend $(EXEC).opt

$(EXEC): $(SOURCES:.ml=.cmo)
	$(OCAMLFIND) $(OCAMLC) -custom  -o $(EXEC) $(LIBS) $(SOURCES:.ml=.cmo)

$(EXEC).opt: $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) -o $(EXEC) $(LIBS:.cma=.cmxa) $(SOURCES:.ml=.cmx)

.ml.cmx:
	$(OCAMLFIND) $(OCAMLOPT) -c $<

.ml.cmo:
	$(OCAMLFIND) $(OCAMLC) -c $<

.ml.cmi:
	$(OCAMLFIND) $(OCAMLC) -c $<

clean:
	rm -f *.cm[aiox] *.o *.annot tutorial

depend:
	$(OCAMLDEP) *.mli *.ml > .depend
