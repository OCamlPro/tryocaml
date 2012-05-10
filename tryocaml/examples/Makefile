OCAMLC=ocamlfind ocamlc -annot -package lwt,str -pp "camlp4o $(JS)/lib/syntax/pa_js.cmo"
OCAMLDEP=ocamldep
MLOBJS= cookies.cmo moving_box2.cmo moving_box.cmo
JS=../js_of_ocaml
OCPJS=../ocp-jslib
INCLUDES= -I $(JS)/lib -I $(OCPJS)

all : $(MLOBJS)

%.cmo: %.ml
	$(OCAMLC) $(INCLUDES) -c $<
%.cmi: %.mli
	$(OCAMLC) $(INCLUDES) -c $<

clean::
	rm -f *.cm* *.annot *~

depend:
	$(OCAMLDEP) -pp "camlp4o $(JS)/lib/syntax/pa_js.cmo" $(INCLUDES) *.ml *.mli > .depend
