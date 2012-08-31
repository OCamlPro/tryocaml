ROOT=..
TRYOCAML_NAME=try-ocaml
LESSON_SET=$(ROOT)/ocaml-lessons

TRYOCAML_EXTRA_CMIS=
TRYOCAML_EXTRA_CMAS=
TRYOCAML_EXTRA_CMOS=
TRYOCAML_EXTRA_MODULES=
TRYOCAML_EXTRA_INCLUDES=

include $(ROOT)/Makefile.config
include $(ROOT)/Makefile.tryocaml
include $(ROOT)/Makefile.rules

upload:
	$(SCP) -C index.html webmaster@ssh.ocamlpro.com:/home/try.ocamlpro.com/www/index.html
	$(SCP) -C try-ocaml.js webmaster@ssh.ocamlpro.com:/home/try.ocamlpro.com/www/try-ocaml.js
	$(SCP) -C style.css webmaster@ssh.ocamlpro.com:/home/try.ocamlpro.com/www/style.css