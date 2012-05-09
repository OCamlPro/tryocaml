OCAMLC=ocamlfind ocamlc -annot -package lwt,str -pp "camlp4o $(JS)/lib/syntax/pa_js.cmo"
OCAMLDEP=ocamldep

MLOBJS= utils.cmo button.cmo cookie.cmo dragnDrop.cmo
MLINTFS= $(MLOBJS:.cmo=.cmi)

JS=../js_of_ocaml
JS_CMIS=$(JS)/lib/*.cmi
INCLUDES= -I $(JS)/lib
LIBNAME=ocp-jslib


all : $(LIBNAME).cma

$(LIBNAME).cma: $(MLINTFS) $(MLOBJS)
	$(OCAMLC) -a -o $(LIBNAME).cma $(MLOBJS)

%.cmo: %.ml
	$(OCAMLC) $(INCLUDES) -c $<
%.cmi: %.mli
	$(OCAMLC) $(INCLUDES) -c $<

clean::
	rm -f *.cm* *.annot *~

depend:
	$(OCAMLDEP) -pp "camlp4o $(JS)/lib/syntax/pa_js.cmo" $(INCLUDES) *.ml *.mli > .depend
