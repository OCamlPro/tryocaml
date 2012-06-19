ROOT=..
TRYOCAML_NAME=try-js_of_ocaml
LESSON_SET=$(ROOT)/js_of_ocaml-lessons

TRYOCAML_EXTRA_CMIS=
TRYOCAML_EXTRA_CMAS=
TRYOCAML_EXTRA_INCLUDES=-I $(OCPJSLIB_DIR)
TRYOCAML_EXTRA_MODULES= \
  css \
  dom \
  dom_events \
  dom_html \
  event_arrows \
  file \
  firebug \
  form \
  js \
  json \
  deriving_Json \
  lwt_js \
  regexp \
  url \
  xmlHttpRequest \
  \
  utils \
  button \
  cookie \
  dragnDrop

include $(ROOT)/Makefile.config
include $(ROOT)/Makefile.tryocaml
include $(ROOT)/Makefile.rules

upload:
	$(SCP) -C index.html webmaster@ssh.ocamlpro.com:/home/try.ocamlpro.com/www/js_of_ocaml/index.html
	$(SCP) -C try-js_of_ocaml.js webmaster@ssh.ocamlpro.com:/home/try.ocamlpro.com/www/js_of_ocaml/try-js_of_ocaml.js
	$(SCP) -C style.css webmaster@ssh.ocamlpro.com:/home/try.ocamlpro.com/www/js_of_ocaml/style.css