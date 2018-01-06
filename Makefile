all:
	$(MAKE) -C src all
	cp src/Main ./latc_x86

clean:
	$(MAKE) -C src clean
	rm -f latc_x86
