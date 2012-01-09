# remove this variable to use the default ocaml toplevel
INCLUDE_TOPLEVELLIB= -I toplevellib

COMPILER=js_of_ocaml
LIBNAME=js_of_ocaml
NAME=toplevel
OBJS=tutorial/tutorial.cma ocaml-num/ocaml-num.cma toplevel.cmo
JS=js_of_ocaml-patched
INCLUDES= -I $(JS)/lib -I $(JS)/compiler -I tutorial $(INCLUDE_TOPLEVELLIB) -I ocaml-num

all: compiler tuto num $(NAME).js

compiler:
	$(MAKE) -C $(JS) all

#include $(JS)/Makefile.conf

COMP=$(JS)/compiler/$(COMPILER)
#COMP=$(LIBRARY)
JSFILES=$(JS)/runtime/runtime.js $(JS)/runtime/weak.js toplevel_runtime.js
OCAMLC=ocamlfind ocamlc -annot -package lwt,str -pp "camlp4o $(JS)/lib/syntax/pa_js.cmo"
STDLIB= $(JS)/lib/$(LIBNAME).cma $(JS)/compiler/compiler.cma  toplevellib.cma
EXPUNGE=$(shell ocamlc -where)/expunge
# Removed gc and sys
STDLIB_MODULES=\
  arg \
  array \
  arrayLabels \
  buffer \
  callback \
  camlinternalLazy \
  camlinternalMod \
  camlinternalOO \
  char \
  complex \
  digest \
  filename \
  format \
  genlex \
  hashtbl \
  int32 \
  int64 \
  lazy \
  lexing \
  list \
  listLabels \
  map \
  marshal \
  moreLabels \
  nativeint \
  obj \
  oo \
  parsing \
  pervasives \
  printexc \
  printf \
  queue \
  random \
  scanf \
  set \
  sort \
  stack \
  stdLabels \
  stream \
  string \
  stringLabels \
  weak \
  n \
  big_int \
  num \
  big \
  tutorial \

PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop

$(NAME).js: $(NAME).byte $(JSFILES) toplevellib/toplevellib.cma
	$(COMP) $(INCLUDES) -toplevel -linkall -noinline -noruntime $(JSFILES) $(NAME).byte $(OPTIONS)

$(NAME).byte: $(OBJS) $(JS)/compiler/compiler.cma toplevellib/toplevellib.cma
	$(OCAMLC) $(INCLUDES) -linkall -package str -linkpkg -o $@.tmp $(STDLIB) $(OBJS)
	$(EXPUNGE) $@.tmp $@ $(PERVASIVES)
	rm -f $@.tmp

%.cmo: %.ml
	$(OCAMLC) $(INCLUDES) -c $<

%.cmi:$(JS)/compiler/compiler.cma

$(JS)/compiler/compiler.cma:
	$(MAKE) -C $(JS)/compiler compiler.cma

errors.cmi: errors.mli
	$(OCAMLC) $(INCLUDES) -c $<

tuto:
	$(MAKE) -C tutorial all

num:
	$(MAKE) -C ocaml-num all

clean::
	rm -f *.cm[io] $(NAME).byte *.annot *~
	$(MAKE) -C tutorial clean
	$(MAKE) -C $(JS) clean

depend:
	ocamldep -pp "camlp4o $(JS)/lib/syntax/pa_js.cmo" $(INCLUDES) *.ml *.mli > .depend

toplevel.cmo: toplevellib/*.cmi

include .depend
