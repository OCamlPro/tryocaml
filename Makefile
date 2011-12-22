all:
	$(MAKE) -C tryocaml

clean:
	$(MAKE) -C tryocaml clean

depend:
	$(MAKE) -C depend

update-lessons:
	touch lessons.html
	$(MAKE) -C tryocaml
