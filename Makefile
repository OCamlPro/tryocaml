

all: Makefile.config
	$(MAKE) -C cmicomp
	ocp-build -scan try-make-lessons
	$(MAKE) -C try-ocaml
	$(MAKE) -C try-js_of_ocaml

Makefile.config:
	./configure

clean:
	$(MAKE) -C cmicomp clean
	$(MAKE) -C try-ocaml clean
	$(MAKE) -C try-js_of_ocaml clean

update-lessons:
	touch ocaml-lessons/goodies.ml
	$(MAKE) -C try-ocaml

upload:
	$(MAKE) -C try-ocaml upload
	$(MAKE) -C try-js_of_ocaml upload
