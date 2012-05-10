ROOT=..

include $(ROOT)/Makefile.config
include $(ROOT)/Makefile.jsconfig

MLOBJS= utils.cmo button.cmo cookie.cmo dragnDrop.cmo
MLINTFS= $(MLOBJS:.cmo=.cmi)

INCLUDES= -I $(JS_DIR)/lib
LIBNAME=ocp-jslib


all : $(LIBNAME).cma

$(LIBNAME).cma: $(MLINTFS) $(MLOBJS)
	$(OCAMLC) -a -o $(LIBNAME).cma $(MLOBJS)

include $(ROOT)/Makefile.rules
