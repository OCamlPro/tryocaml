# cmicomp is a pure bytecode copy of ocp-cmi-compress, itself a restriction
# of ocp-cmi, to avoid non dynamic dependencies

JS_CMIS= \
CSS.cmi		event_arrows.cmi  js.cmi      typed_array.cmi    \
dom.cmi		file.cmi	  json.cmi    url.cmi            \
dom_events.cmi	firebug.cmi	  lwt_js.cmi  webGL.cmi          \
dom_html.cmi	form.cmi	  regexp.cmi  xmlHttpRequest.cmi

ROOT=..
include $(ROOT)/Makefile.config
include $(TOPLEVELLIB_DIR)/Makefile.config
CMICOMP=$(TOPLEVELLIB_DIR)/cmicomp

all: $(CMICOMP) $(JS_CMIS)


$(JS_CMIS):  $(JS_DIR)/*.cmi
	cp -f $(JS_DIR)/*.cmi .
	cp -f $(JS_DIR)/js_of_ocaml.cma .
	ocamlrun $(CMICOMP) $(CMICOMP_FLAGS) $(JS_CMIS)

clean:
	rm -f *.cmi *.cmi.old
