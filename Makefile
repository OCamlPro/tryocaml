all:
	$(MAKE) -C tryocaml

clean:
	$(MAKE) -C tryocaml clean

depend:
	$(MAKE) -C depend

update-lessons:
	touch lessons/goodies.ml
	$(MAKE) -C tryocaml tuto tryocaml
