all: build lib

build:
	$(MAKE) -C src all
	cp src/Main ./compiler

lib: lib/runtime.o

lib/runtime.o: lib/runtime.c
	gcc -m32 -c lib/runtime.c -o lib/runtime.o

clean:
	$(MAKE) -C src clean
	rm -f compiler lib/runtime.o
