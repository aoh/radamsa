DESTDIR=
PREFIX=/usr
BINDIR=/bin
CFLAGS=-Wall -O3
CC=gcc
#OFLAGS=--usual-suspects
OFLAGS=--native
W32GCC=i586-mingw32msvc-gcc # sudo apt-get install mingw32 @ debian squeeze

you: deps bin/radamsa .seal-of-quality

bin/radamsa: radamsa.c
	$(CC) $(CFLAGS) -o bin/radamsa radamsa.c

bin/radamsa.exe: radamsa.c
	which $(W32GCC)
	$(W32GCC) $(CFLAGS) -o bin/radamsa.exe radamsa.c -lwsock32

radamsa.c: radamsa.l
	ol $(OFLAGS) -o radamsa.c radamsa.l

install: bin/radamsa
	test -d $(DESTDIR)$(PREFIX)/bin || mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp bin/radamsa $(DESTDIR)$(PREFIX)/bin
	test -d $(DESTDIR)$(PREFIX)/share/man/man1 || mkdir -p $(DESTDIR)$(PREFIX)/share/man/man1
	cat doc/radamsa.1 | gzip -9 > $(DESTDIR)$(PREFIX)/share/man/man1/radamsa.1.gz

bytecode:
	# make a plain bytecode dump, which gcc compiles *much* faster
	make OFLAGS="" bin/radamsa

test:
	test -f .seal-of-quality && rm .seal-of-quality || true
	make .seal-of-quality

clean:
	-rm radamsa.c bin/* .seal-of-quality

.seal-of-quality: bin/radamsa
	sh tests/run bin/radamsa
	touch .seal-of-quality

todo:
	ol -n *.l

deps:
	which ol || { echo "you need owl from https://github.com/aoh/owl-lisp"; false; }
	which $(CC) || { echo "you need a C-compiler (default gcc)"; false; }

uninstall:
	rm $(DESTDIR)$(PREFIX)/bin/radamsa || echo "no radamsa"
	rm $(DESTDIR)$(PREFIX)/share/man/man1/radamsa.1.gz || echo "no manpage"

.PHONY: todo you install clean test bytecode uninstall
