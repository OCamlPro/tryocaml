all: compiler library runtime tutorial

include Makefile.conf

.PHONY: compiler library runtime tutorial

compiler:
	$(MAKE) -C compiler

library:
	$(MAKE) -C lib

runtime:
	$(MAKE) -C runtime

clean:
	$(MAKE) -C compiler clean
	$(MAKE) -C lib clean
	$(MAKE) -C runtime clean

realclean: clean
	find . -name "*~" -print | xargs rm -f
