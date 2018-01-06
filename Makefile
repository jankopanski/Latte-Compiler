all: build lib

build:
	$(MAKE) -C src all
	cp src/Main ./compiler

lib: lib/lib.o

lib/lib.o: lib/lib.c
	gcc -m32 -c lib/lib.c -o lib/lib.o

clean:
	$(MAKE) -C src clean
	rm -f compiler lib/lib.o
