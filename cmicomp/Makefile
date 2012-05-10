
JS_CMIS= \
CSS.cmi		event_arrows.cmi  js.cmi      typed_array.cmi    \
dom.cmi		file.cmi	  json.cmi    url.cmi            \
dom_events.cmi	firebug.cmi	  lwt_js.cmi  webGL.cmi          \
dom_html.cmi	form.cmi	  regexp.cmi  xmlHttpRequest.cmi

all: cmicomp $(JS_CMIS)

ROOT=..
include $(ROOT)/Makefile.config

CMICOMP=./cmicomp

$(JS_CMIS):  $(JS_DIR)/lib/*.cmi
	cp -f $(JS_DIR)/lib/*.cmi .
	cp -f $(JS_DIR)/lib/js_of_ocaml.cma .
	$(CMICOMP) -without-log $(JS_CMIS)
