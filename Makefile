all: compiler library runtime tutorial src

include Makefile.conf

.PHONY: compiler library runtime tutorial src

src:
	$(MAKE) -C src

compiler:
	$(MAKE) -C compiler

library:
	$(MAKE) -C lib

runtime:
	$(MAKE) -C runtime

tutorial:
	$(MAKE) -C tutorial

depend:
	$(MAKE) -C src depend

clean:
	$(MAKE) -C src clean
	$(MAKE) -C compiler clean
	$(MAKE) -C lib clean
	$(MAKE) -C runtime clean
	$(MAKE) -C tutorial clean

realclean: clean
	find . -name "*~" -print | xargs rm -f
