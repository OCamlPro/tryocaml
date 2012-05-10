
all: byte

OCAMLC = ocamlc
OCAMLDEP = ocamldep
SOURCES= lessons.ml n.ml tutorial.ml

.SUFFIXES: .ml .mli .cmo .cmi .cmx

all: lessons.ml byte

lessons.ml: make_lessons.ml ../../lessons/goodies.ml
	ocamlc -o make_lessons unix.cma make_lessons.ml
	./make_lessons > lessons.ml

CMOS= $(SOURCES:.ml=.cmo)

byte: tutorial.cma

tutorial.cma: $(CMOS)
	$(OCAMLC) -a -o tutorial.cma $(CMOS)

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

clean:
	rm -f *.cm[aiox] *.o *.annot *~ lessons.ml make_lessons

depend: $(SOURCES)
	$(OCAMLDEP) $(SOURCES) > .depend

-include .depend
