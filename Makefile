DESTDIR=
PREFIX=/usr
BINDIR=/bin
CFLAGS=-Wall -O3
OFLAGS=-O1
W32GCC=i586-mingw32msvc-gcc # sudo apt-get install mingw32 @ debian squeeze

everything: bin/radamsa .seal-of-quality

bin/radamsa: radamsa.c
	mkdir -p bin
	$(CC) $(CFLAGS) -o bin/radamsa radamsa.c

bin/radamsa.exe: radamsa.c
	which $(W32GCC)
	$(W32GCC) $(CFLAGS) -o bin/radamsa.exe radamsa.c -lwsock32

radamsa.c: rad/*.scm
	ol $(OFLAGS) -o radamsa.c rad/main.scm

install: bin/radamsa
	-mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp bin/radamsa $(DESTDIR)$(PREFIX)/bin
	-mkdir -p $(DESTDIR)$(PREFIX)/share/man/man1
	cat doc/radamsa.1 | gzip -9 > $(DESTDIR)$(PREFIX)/share/man/man1/radamsa.1.gz

clean:
	-rm radamsa.c bin/* .seal-of-quality

.seal-of-quality: bin/radamsa
	-mkdir -p tmp
	sh tests/run bin/radamsa
	touch .seal-of-quality

get-owl:
	# need to install owl to be able to compile radamsa
	# this may take a moment depending on your machine
	-git clone http://haltp.org/git/owl-lisp.git
	-cd owl-lisp && git pull 
	cd owl-lisp && make && sudo make install

# standalone build for shipping
standalone:
	-rm radamsa.c # likely old version
	make radamsa.c
   # compile without seccomp and use of syscall
	diet gcc -DNO_SECCOMP -O3 -Wall -o bin/radamsa radamsa.c
	
deps:
	which $(CC) || { echo "you need a C-compiler (default gcc)"; false; }
	which ol || make get-owl

uninstall:
	rm $(DESTDIR)$(PREFIX)/bin/radamsa || echo "no radamsa"
	rm $(DESTDIR)$(PREFIX)/share/man/man1/radamsa.1.gz || echo "no manpage"

.PHONY: todo you install clean test bytecode uninstall get-owl standalone
