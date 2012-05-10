all:
	$(MAKE) -C js_of_ocaml
	$(MAKE) -C js_of_ocaml/compiler compiler.cma
	$(MAKE) -C toplevel
	$(MAKE) -C ocaml-num
	$(MAKE) -C tutorial
	$(MAKE) -C try-ocaml
	$(MAKE) -C try-js_of_ocaml

clean:
	$(MAKE) -C tryocaml clean

depend:
	$(MAKE) -C depend

update-lessons:
	touch lessons/goodies.ml
	$(MAKE) -C tryocaml tuto tryocaml
