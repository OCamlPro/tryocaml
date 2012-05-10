byte: toplevel.cma

ROOT=..
include ../Makefile.config
# remove this variable to use the default ocaml toplevel
INCLUDE_TOPLEVELLIB= -I $(TOPLEVELLIB_DIR)/
TOPLEVELLIB=$(TOPLEVELLIB_DIR)/

TOPLEVEL_CMIS=toplevel.cmi
TOPLEVEL_CMOS= toplevel.cmo topmain.cmo

toplevel.cma: $(TOPLEVEL_CMOS) $(TOPLEVEL_CMIS)
	$(OCAMLC) -a -o toplevel.cma $(TOPLEVEL_CMOS)

INCLUDES= \
   -I $(JS_DIR)/lib -I $(JS_DIR)/compiler \
   -I $(TUTORIAL_DIR)/ \
   $(INCLUDE_TOPLEVELLIB) \
   -I $(OCAMLNUM_DIR) \
   -I $(TOPLEVEL_DIR) \
   -I .

PP=-pp "camlp4o $(JS_DIR)/lib/syntax/pa_js.cmo"

include ../Makefile.rules
