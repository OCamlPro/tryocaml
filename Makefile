all:
	$(MAKE) -C tryocaml
	$(MAKE) -C ocaml-num
	$(MAKE) -C tutorial
	$(MAKE) -C ocaml-lessons
	$(MAKE) -C ml2js-lessons

clean:
	$(MAKE) -C tryocaml clean

depend:
	$(MAKE) -C depend

update-lessons:
	touch lessons/goodies.ml
	$(MAKE) -C tryocaml tuto tryocaml
