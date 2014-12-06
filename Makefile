DESTDIR=
PREFIX=/usr
BINDIR=/bin
CFLAGS=-Wall -O2
OFLAGS=-O2
OL=owl-lisp/bin/ol

W32GCC=i586-mingw32msvc-gcc # sudo apt-get install mingw32 @ debian squeeze

everything: bin/radamsa .seal-of-quality

bin/radamsa: radamsa.c
	mkdir -p bin
	$(CC) $(CFLAGS) -o bin/radamsa radamsa.c

fasl: radamsa.fasl
	echo "#!/usr/bin/owl-vm" > fasl
	cat radamsa.fasl >> fasl
	chmod +x fasl

radamsa.fasl: rad/*.scm
	$(OL) -o radamsa.fasl rad/main.scm

bin/radamsa.exe: radamsa.c
	which $(W32GCC)
	$(W32GCC) $(CFLAGS) -o bin/radamsa.exe radamsa.c -lwsock32

radamsa.c: rad/*.scm
	make get-owl
	$(OL) $(OFLAGS) -o radamsa.c rad/main.scm

install: bin/radamsa
	-mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp bin/radamsa $(DESTDIR)$(PREFIX)/bin
	-mkdir -p $(DESTDIR)$(PREFIX)/share/man/man1
	cat doc/radamsa.1 | gzip -9 > $(DESTDIR)$(PREFIX)/share/man/man1/radamsa.1.gz

clean:
	-rm radamsa.c bin/radamsa .seal-of-quality
	-cd owl-lisp && make clean

.seal-of-quality: bin/radamsa
	-mkdir -p tmp
	sh tests/run bin/radamsa
	touch .seal-of-quality

get-owl:
	# fetching and building owl to build radamsa
	# this may take a few minutes on first build
	-git clone https://github.com/aoh/owl-lisp.git
	-cd owl-lisp && git pull 
	cd owl-lisp && make

todo:
	grep -n "^ *;;* *todo:" rad/* | sed -e 's/: *;;* *todo:/ →/'

# standalone build for shipping
standalone:
	-rm radamsa.c # likely old version
	make radamsa.c
   # compile without seccomp and use of syscall
	diet gcc -DNO_SECCOMP -O3 -Wall -o bin/radamsa radamsa.c

# a quick to compile vanilla bytecode executable
bytecode:
	$(OL) -O0 -x c -o - rad/main.scm | $(CC) -O2 -x c -o bin/radamsa -
	-mkdir -p tmp
	sh tests/run bin/radamsa

# a simple mutation benchmark
benchmark: bin/radamsa
	tests/benchmark bin/radamsa

uninstall:
	rm $(DESTDIR)$(PREFIX)/bin/radamsa || echo "no radamsa"
	rm $(DESTDIR)$(PREFIX)/share/man/man1/radamsa.1.gz || echo "no manpage"

.PHONY: todo you install clean test bytecode uninstall get-owl standalone
