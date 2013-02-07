

all: Makefile.config
	$(MAKE) -C cmicomp
	ocp-build -scan -meta - try-make-lessons
#	$(MAKE) -C ocp-jslib
#	$(MAKE) -C ocaml-num
#	$(MAKE) -C toplevel
#	$(MAKE) -C tutorial
	$(MAKE) -C try-ocaml
	$(MAKE) -C try-js_of_ocaml

Makefile.config:
	./configure

clean:
	$(MAKE) -C cmicomp clean
#	$(MAKE) -C ocp-jslib clean
#	$(MAKE) -C toplevel clean
#	$(MAKE) -C ocaml-num clean
#	$(MAKE) -C toplevel clean
#	$(MAKE) -C tutorial clean
	$(MAKE) -C try-ocaml clean
	$(MAKE) -C try-js_of_ocaml clean
	rm -rf _obuild

depend:
	$(MAKE) -C ocp-jslib depend
	$(MAKE) -C toplevel depend
	$(MAKE) -C tutorial depend
	$(MAKE) -C try-ocaml depend
	$(MAKE) -C try-js_of_ocaml depend

update-lessons:
	touch ocaml-lessons/goodies.ml
	$(MAKE) -C try-ocaml

upload:
	$(MAKE) -C try-ocaml upload
	$(MAKE) -C try-js_of_ocaml upload
