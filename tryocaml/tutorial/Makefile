
all: byte

OCAMLC = ocamlc
OCAMLDEP = ocamldep
SOURCES= lessons.mli lessons.ml tutorial.mli tutorial.ml

.SUFFIXES: .ml .mli .cmo .cmi .cmx

all: lessons.ml byte

lessons.ml: make_lessons.ml ../../lessons.txt
	ocamlc -o make_lessons unix.cma make_lessons.ml
	./make_lessons > lessons.ml

OBJS= $(SOURCES:.mli=.cmi) $(SOURCES:.ml=.cmo)

byte: $(OBJS)

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

clean:
	rm -f *.cm[aiox] *.o *.annot *~ lessons.ml make_lessons

depend: $(SOURCES)
	$(OCAMLDEP) $(SOURCES) > .depend
