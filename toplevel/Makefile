byte: toplevel.cma

ROOT=..
include ../Makefile.config
include ../Makefile.jsconfig

# remove this variable to use the default ocaml toplevel
INCLUDE_TOPLEVELLIB= -I $(TOPLEVELLIB_DIR)/
TOPLEVELLIB=$(TOPLEVELLIB_DIR)/

TOPLEVEL_CMIS=toplevel.cmi
TOPLEVEL_CMOS= toplevel.cmo topmain.cmo

toplevel.cma: $(TOPLEVEL_CMOS) $(TOPLEVEL_CMIS)
	$(OCAMLC) -a -o toplevel.cma $(TOPLEVEL_CMOS)

INCLUDES= \
   -I $(CMICOMP_DIR) \
   -I $(JS_DIR) \
   -I $(TUTORIAL_DIR)/ \
   $(INCLUDE_TOPLEVELLIB) \
   -I $(OCAMLNUM_DIR) \
   -I $(OCPJSLIB_DIR) \
   -I $(JS_COMPILER_LIB) js_of_ocaml_compiler.cma

include ../Makefile.rules
