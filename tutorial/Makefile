
all: byte

ROOT=..
include ../Makefile.config

SOURCES= lessons.mli
TARGETS=make_lessons

all: byte

lessons.ml: make_lessons.ml ../lessons/goodies.ml
	./make_lessons > lessons.ml

CMOS=  n.cmo tutorial.cmo

byte: tutorial.cma make_lessons

make_lessons: make_lessons.ml
	ocamlc -o make_lessons unix.cma make_lessons.ml

tutorial.cma: $(CMOS)
	$(OCAMLC) -a -o tutorial.cma $(CMOS)

include ../Makefile.rules
