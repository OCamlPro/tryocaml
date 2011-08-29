
NAME=toplevel
OBJS=toplevel.cmo

all: $(NAME).js

include ../Makefile.conf
include $(JS_OF_OCAML_PATH)/Makefile.conf
-include $(JS_OF_OCAML_PATH)/Makefile.local

COMP=$(JS_OF_OCAML_PATH)/compiler/$(COMPILER)
JSFILES=$(JS_OF_OCAML_PATH)/runtime/runtime.js $(JS_OF_OCAML_PATH)/runtime/weak.js toplevel_runtime.js
OCAMLC=ocamlfind ocamlc -package lwt,str -pp "camlp4o $(JS_OF_OCAML_PATH)/lib/syntax/pa_js.cmo" -I $(JS_OF_OCAML_PATH)/lib -I $(JS_OF_OCAML_PATH)/compiler
STDLIB=$(JS_OF_OCAML_PATH)/lib/$(LIBNAME).cma $(JS_OF_OCAML_PATH)/compiler/compiler.cma toplevellib.cma

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
  weak
PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop

#toplevel.byte: $(OBJS:cmx=cmo) toplevel.cmo
#	ocamlfind ocamlc -linkall -g -package str -linkpkg toplevellib.cma -o $@.tmp $^

$(NAME).js: $(NAME).byte $(COMP) $(JSFILES)
	$(COMP) -toplevel -noinline -noruntime $(JSFILES) $(NAME).byte $(OPTIONS)

$(NAME).byte: $(OBJS) $(JS_OF_OCAML_PATH)/compiler/compiler.cma
	$(OCAMLC) -linkall -package str -linkpkg -o $@.tmp $(STDLIB) $(OBJS)
	/usr/lib/ocaml/expunge $@.tmp $@ $(PERVASIVES)
	rm -f $@.tpm

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: $(JS_OF_OCAML_PATH)/compiler/compiler.cma

$(JS_OF_OCAML_PATH)/compiler/compiler.cma:
	$(MAKE) -C $(JS_OF_OCAML_PATH)/compiler compiler.cma

errors.cmi: errors.mli
	$(OCAMLC) -c $<

clean::
	rm -f *.cm[io] $(NAME).byte $(NAME).js

depend:
	ocamldep -pp "camlp4o $(JS_OF_OCAML_PATH)/lib/syntax/pa_js.cmo" -I $(JS_OF_OCAML_PATH)/compiler *.ml *.mli > .depend

include .depend
