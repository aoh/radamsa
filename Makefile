DESTDIR=
PREFIX=/usr
BINDIR=/bin
CFLAGS?=-Wall -O2
LDFLAGS?=
OFLAGS=-O2
OWL=ol-0.1.13
OWLURL=https://github.com/aoh/owl-lisp/files/449350
USR_BIN_OL=/usr/bin/ol

everything: bin/radamsa

build_radamsa:
	test -x $(USR_BIN_OL)
	$(USR_BIN_OL) $(OFLAGS) -o radamsa.c rad/main.scm
	mkdir -p bin
	$(CC) $(CFLAGS) $(LDFLAGS) -o bin/radamsa radamsa.c

bin/radamsa: radamsa.c
	mkdir -p bin
	$(CC) $(CFLAGS) $(LDFLAGS) -o bin/radamsa radamsa.c

radamsa.c: rad/*.scm
	test -x bin/ol || make bin/ol
	bin/ol $(OFLAGS) -o radamsa.c rad/main.scm

radamsa.fasl: rad/*.scm bin/ol
	bin/ol -o radamsa.fasl rad/main.scm

$(OWL).c:
	test -f $(OWL).c.gz || wget $(OWLURL)/$(OWL).c.gz

bin/ol: $(OWL).c
	gzip -d < $(OWL).c.gz > $(OWL).c
	mkdir -p bin
	cc -O2 -o bin/ol $(OWL).c

install: bin/radamsa
	-mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp bin/radamsa $(DESTDIR)$(PREFIX)/bin
	-mkdir -p $(DESTDIR)$(PREFIX)/share/man/man1
	cat doc/radamsa.1 | gzip -9 > $(DESTDIR)$(PREFIX)/share/man/man1/radamsa.1.gz

clean:
	-rm radamsa.c bin/radamsa .seal-of-quality
	-rm bin/ol $(OWL).c.gz $(OWL).c

test: .seal-of-quality

.seal-of-quality: bin/radamsa
	-mkdir -p tmp
	sh tests/run bin/radamsa
	touch .seal-of-quality

# standalone build for shipping
standalone:
	-rm radamsa.c # likely old version
	make radamsa.c
   # compile without seccomp and use of syscall
	diet gcc -DNO_SECCOMP -O3 -Wall -o bin/radamsa radamsa.c

# a quick to compile vanilla bytecode executable
bytecode: bin/ol
	bin/ol -O0 -x c -o - rad/main.scm | $(CC) -O2 -x c -o bin/radamsa -
	-mkdir -p tmp
	sh tests/run bin/radamsa

# a simple mutation benchmark
benchmark: bin/radamsa
	tests/benchmark bin/radamsa

uninstall:
	rm $(DESTDIR)$(PREFIX)/bin/radamsa || echo "no radamsa"
	rm $(DESTDIR)$(PREFIX)/share/man/man1/radamsa.1.gz || echo "no manpage"

.PHONY: todo you install clean test bytecode uninstall get-owl standalone
