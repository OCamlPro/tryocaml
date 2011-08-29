all: src

include Makefile.conf

.PHONY: src

src:
	$(MAKE) -C src

depend:
	$(MAKE) -C src depend

clean:
	$(MAKE) -C src clean
