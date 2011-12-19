
include ../../Makefile.conf

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLDOC = ocamldoc
OCAMLFIND = ocamlfind 
EXEC=tutorial
LIBS=unix.cma str.cma
SOURCES=utils.ml \
	tutorial.mli tutorial.ml
OPTIONS=-annot
DOCS_DIR=doc
HTML=$(DOCS_DIR)/html
LATEX=$(DOCS_DIR)/latex
MAN=$(DOCS_DIR)/man

.SUFFIXES: .ml .mli .cmo .cmi .cmx

all: byte opt docs

byte: depend $(EXEC)

opt : depend $(EXEC).opt

$(EXEC): $(SOURCES:.mli=.cmi) $(SOURCES:.ml=.cmo)
	$(OCAMLFIND) $(OCAMLC) -custom -o $(EXEC) $(LIBS)  $(SOURCES:.ml=.cmo)

$(EXEC).opt: $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) $(OPTIONS) -o $(EXEC).opt $(LIBS:.cma=.cmxa) $(SOURCES:.ml=.cmx)

.ml.cmx:
	$(OCAMLFIND) $(OCAMLOPT) $(OPTIONS) -c $<

.ml.cmo:
	$(OCAMLFIND) $(OCAMLC) $(OPTIONS) -c $<

.mli.cmi:
	$(OCAMLFIND0) $(OCAMLC) $(OPTIONS) -c $<

docs:
	mkdir -p $(HTML) $(LATEX) $(MAN)
	$(OCAMLFIND) $(OCAMLDOC) -html -d $(HTML) $(SOURCES)
	$(OCAMLFIND) $(OCAMLDOC) -latex -d $(LATEX) -o $(LATEX)/tutorial.tex $(SOURCES)
	$(OCAMLFIND) $(OCAMLDOC) -man -d $(MAN) $(SOURCES)

clean:
	rm -f *.cm[aiox] *.o *.annot tutorial tutorial.opt 
	rm -rf doc/* doc/

depend:
	$(OCAMLDEP) *.mli *.ml > .depend
