# A Crash Course to Radamsa

Radamsa is a test case generator for robustness testing, aka a fuzzer. It can be used to test how well a program can stand malformed and potentially malicious inputs. It works by making files which are interestingly different from given typically valid sample files. The modified files are then given to the target program, either as such or by some script. The main selling points of radamsa as opposed to other fuzzers are, that it is extremely easy to get running on most machines, is easy to script from command line, and has already been used to find a slew of security issues in programs you are likely using right now.

## Nutshell:

```
 $ # please please please fuzz your programs. here is one way to get data for it:
 $ sudo apt-get install gcc curl
 $ curl http://haltp.org/download/radamsa-0.4.tar.gz \
     | tar -zxvf - && cd radamsa-0.4 && make && sudo make install
 $ # signatures are also available at http://haltp.org/download
 $ cat file | radamsa > file.fuzzed
```

## What the Fuzz

Computer programs are extremely complex beasts. In most programs extremely small errors in the code, compiler, library or any other component can lead to a vulnerability in the program, which malicious parties often can use to gain undesired powers. Fuzzing is one of the common techniques to find such weaknesses.

Many mature programs come with an extensive suite of tests which are used to ensure that the things which should work in it do indeed work as expected. This can be called positive testing. Typically a positive test assumes that the program works correctly for all certain kinds of inputs, and tests his by giving it a fixed or random case and checks that the program indeed gives the right answer. Similarly there are things that should not happen in a program, like crashing or getting stuck. Checking this tends to a significantly harder problem, because we know what the outcome would be, but have no idea as to what might cause it. Both kinds of tests are actually just approximations of proving properties about the program, but this is not yet a viable option because the semantics of current programming languages are quite messy.

Fuzzing is one of the techniques used mainly for negative testing. It simply means generating a stream of more and less random data for the program being tested, and waiting for the thing that should not happen. Sometimes fuzzing refers to the whole setup used to do this, but in this document a fuzzer means just the tool which generates the stream of test cases.

There are many kinds of fuzzers. A common way to classify them is to map the amount of data the fuzzer has about the target program or appliance and its internals to a grayscale color. A theoretically fully black-box fuzzer doesn't know anything about anything and thus only generates data out of nothing. A practical black-box fuzzer is given samples of what kind of data a program usually processes, but is then assumed to go about generating interesting data without any feedback as to what is processing the data or even how well the previous attempts have worked. Gray-box fuzzers have some knowledge about the target and running of the tests, and a white-box fuzzer often means one that is given the full spec of data formats, program, the binary in question and/or runtime tracing data possibly with control of the runtime behavior.

Intuitively black-box fuzzers are easy to use and general purpose, but don't dig deep into the state space of a program, unless they are lucky, while white-box fuzzers are harder to use and tailored for some particular task, and have what it needs to dig deep into the intended functionality. The set of bugs found by different approaches are usually at least somewhat disjoint, so it's not a question of what is the best color, but rather how well different shades are covered on the attack surface.

In addition to fuzzing, other typical approaches to testing include static code analysis, where preferably a program is used to automatically detect possible vulnerabilities in code, and various forms of simulated program running which allow one to step through the possible state space. The pros of fuzzing are that it is often extremely simple to start testing a product, scaling is easy, testing depth can be increased later without need for changing the test harness, it requires relatively little human time and each bug comes automatically paired with a proof of concept that triggers the undesired condition.

All code should be tested in as many ways as possible and as early as possible. An important thing to keep in mind is that an opportunistic hacker will most likely start by doing some simple black-box fuzzing, so it doesn't help if your product has been extensively unit tested on the function level, but there is a vulnerable third party library, confusion of roles, thread race or another condition which is still easily exposed by black-box tools. Full-program fuzzing is basically unit testing done too late, but you should still always do it.

## Radamsa

Radamsa is a somewhat new kind of black-box fuzzer. It is designed to be a good general purpose tool for developers and vendors who want to test how well their products can withstand malicious inputs. Unlike most similar tools, it is intended to work for just about any kind of data without any extra hacking. It has been used to find previously unknown security vulnerabilities in handling of many kinds of file formats with very different structure, like bmp, png, gif, jpeg, svg, xml, ogg, avi, html, gz, bzip2, tiff, pdf and zip. The main goal is to make a tool that works well no matter what kind of data is thrown at it. By extension, we hope other fuzzers will not to find too many bugs in programs that developers have already fuzzed with radamsa.

The goal of making a truly general purpose fuzzer poses some interesting problems. The current approach is to combine a lot of different kinds of mutations which have turned out to find interesting bugs. Many of the mutations don't assume anything about the structure of the data and rely solely on luck or redundancy in the data, while others check if some commonly used patterns are used and make changes accordingly. Radamsa is thus actually a flock of fuzzers, wrapped in an easy-to-use package, which we hope will be used by also others than us to make programs more secure.

Radamsa is a side product of OUSPG's Protos Genome Project, in which some techniques to automatically analyze and examine the structure of communication protocols were explored. A subset of one of the tools turned out to be a surprisingly effective file fuzzer. Our first prototype black-box fuzzer tools mainly used regular and context-free formal languages to represent the inferred model of the data.

## Requirements

Operating System:
 * GNU/Linux
 * OpenBSD 
 * FreeBSD
 * Mac OS X
 * Windows (experimental)

Software requirements for building from sources:
 * gcc / clang
 * make
 * git

Installation
Instructions for downloading and compiling the current version in a Debian-based Linux are at the top of this page. There should also be static binary and a Windows-executable in the Downloads section. You only need to have the radamsa binary somewhere to use it, but running $ make install will also copy the manual page and make it usable for other users on the system.

## Building Radamsa
```
 $ git clone https://github.com/aoh/radamsa.git
 $ cd radamsa
 $ make
 $ sudo make install
 $ radamsa --help
```

On first build this will also fetch and build owl lisp to compile the Scheme sources to C. This may take a few minutes to complete. If you already have owl, you can build using it by using e.g. $ make OL=/usr/bin/ol.

If the build takes several aeons on your raspberry, you can usually skip a few steps and reduce your C compiler memory load by doing just:
```
 $ git clone https://github.com/aoh/radamsa.git
 $ cd radamsa
 $ git clone https://github.com/aoh/owl-lisp.git
 $ (cd owl-lisp; make bin/vm)
 $ owl-lisp/bin/vm owl-lisp/fasl/init.fasl -O1 -o radamsa.c rad/main.scm
 $ cc -O2 -o radamsa radamsa.c
 $ echo foo | ./radamsa
 foo
 foo
``` 

Radamsa itself is just a single standalone binary. You can move it where you please and remove the rest.

## Building Legacy Radamsa 0.1.9
The 0.1 series of radamsa used a different set of global analysis. While this tool has been used to find dozens of interesting vulnerabilities in the past, we recommend using the newest ones which do similar mutations in sub-linear space. The 0.1 series could take up to 100x the total sample set size of memory and generate data slower than 100Kb/s, while the newer versions work in roughly constant space and generate data an order of magnitude faster.

For small sets of small samples the old one might still be useful. To compile it, use:

```
$ wget https://ouspg.googlecode.com/files/radamsa-0.1.9.c.gz
$ gunzip radamsa-0.1.9.gz
$ gcc -O2 -o radamsa radamsa-0.1.9.c
$ ./radamsa --help
```

## Fuzzing with Radamsa

This section assumes some familiarity with UNIX scripting.

Radamsa can be thought as the cat UNIX tool, which manages to break the data in often interesting ways as it flows through. It has also support for generating more than one output at a time and acting as a TCP server or client, in case such things are needed.

Use of radamsa will be demonstrated by means of small examples. We will use the bc arbitrary precision calculator as an example target program.

In the simplest case, from scripting point of view, radamsa can be used to fuzz data going through a pipe.

```
 $ echo "aaa" | radamsa
 aaaa
```
Here radamsa decided to add one 'a' to the input. Let's try that again.

```
 $ echo "aaa" | radamsa
 ːaaa
```

Now we got another result. By default radamsa will grab a random seed from /dev/urandom if it is not given a specific random state to start from, and you will generally see a different result every time it is started, though for small inputs you might see the same or the original fairly often . The random state to use can be given with the -s parameter, which is followed by a number. Using the same random state will result in the sama data being generated.

```
 $ echo "Fuzztron 2000" | radamsa --seed 4
 Fuzztron 4294967296
```

This particular example was chosen because radamsa happens to choose to use a number mutator, which replaces textual numbers with something else. Programmers might recognize why for example this particular number might be an interesting one to test for.

You can generate more than one output by using the -n parameter as follows:

```
 $ echo "1 + (2 + (3 + 4))" | radamsa --seed 12 -n 4
 1 + (2 + (2 + (3 + 4?)
 1 + (2 + (3 +?4))
 18446744073709551615 + 4)))
 1 + (2 + (3 + 170141183460469231731687303715884105727))
```

There is no guarantee that all of the outputs will be unique. However, when using nontrivial samples, equal outputs tend to be extremely rare.

What we have so far can be used to for example test programs that read input from standard input, as in

```
 $ echo "100 * (1 + (2 / 3))" | radamsa -n 10000 | bc
 [...]
 (standard_in) 1418: illegal character: ^_
 (standard_in) 1422: syntax error
 (standard_in) 1424: syntax error
 (standard_in) 1424: memory exhausted
 [hang]
```

Or the compiler used to compile Radamsa:

```
 $ echo '((lambda (x) (+ x 1)) #x124214214)' | radamsa -n 10000 | ol
 [...]
 > What is 'ó µ'? 
 4901126677
 > $
```

Or to test decompression:

```
 $ gzip -c /bin/bash | radamsa -n 1000 | gzip -d > /dev/null
```

Typically however one might want separate runs for the program for each output. Basic shell scripting makes this easy. Usually we want a test script to run continuously, so we'll use an infinite loop here:

```
 $ gzip -c /bin/bash > sample.gz
 $ while true; do radamsa sample.gz | gzip -d > /dev/null; done
```

Notice that we are here giving the sample as a file instead of running Radamsa in a pipe. Like cat Radamsa will by default write the output to stdout, but unlike cat when given more than one file it will usually use only one or a few of them to create one output. This test will go about throwing fuzzed data against gzip, but doesn't care what happens then. One simple way to find out if something bad happened to a (simple single-threaded) program is to check whether the exit value is greater than 127, which would indicate a fatal program termination. This can be done for example as follows:

```
 $ gzip -c /bin/bash > sample.gz
 $ while true
 do
   radamsa sample.gz > fuzzed.gz
   gzip -dc fuzzed.gz > /dev/null
   test $? -gt 127 && break
 done
```

This will run for as long as it takes to crash gzip, which hopefully is no longer even possible, and the fuzzed.gz can be used to check the issue if the script has stopped. We have found a few such cases, the last one of which took about 3 months to find, but all of them have as usual been filed as bugs and have been promptly fixed by the upstream.

One thing to note is that since most of the outputs are based on data in the given samples (standard input or files given at command line) it is usually a good idea to try to find good samples, and preferably more than one of them. In a more real-world test script radamsa will usually be used to generate more than one output at a time based on tens or thousands of samples, and the consequences of the outputs are tested mostly in parallel, often by giving each of the output on command line to the target program. We'll make a simple such script for bc, which accepts files from command line. The -o flag can be used to give a file name to which radamsa should write the output instead of standard output. If more than one output is generated, the path should have a %n in it, which will be expanded to the number of the output.

```
 $ echo "1 + 2" > sample-1
 $ echo "(124 % 7) ^ 1*2" > sample-2
 $ echo "sqrt((1 + length(10^4)) * 5)" > sample-3
 $ bc sample-* < /dev/null
 3
 10
 5
 $ while true
 do
   radamsa -o fuzz-%n -n 100 sample-*
   bc fuzz-* < /dev/null
   test $? -gt 127 && break
 done
```

This will again run up to obviously interesting times indicated by the large exit value, or up to the target program getting stuck.

In practice many programs fail in unique ways. Some common ways to catch obvious errors are to check the exit value, enable fatal signal printing in kernel and checking if something new turns up in dmesg, run a program under strace, gdb or valgrind and see if something interesting is caught, check if an error reporter process has been started after starting the program, etc.

## Output Options

The examples above all either wrote to standard output or files. One can also ask radamsa to be a TCP client or server by using a special parameter to -o. The output patterns are:

|-o argument | meaning | example |
|------------|---------|---------|
| :port      | act as a TCP server in given port |  # radamsa -o :80 -n inf samples/*.http-resp |
|ip:port     | connect as TCP client to port of ip | $ radamsa -o 127.0.0.1:80 -n inf samples/*.http-req |
| -          | write to stdout |  $ radamsa -o - samples/*.vt100 |
| path       | write to files, %n is testcase # and %s the first suffix | $ radamsa -o test-%n.%s -n 100 samples/*.foo |

Remember that you can use e.g. tcpflow to record TCP traffic to files, which can then be used as samples for radamsa.

## Related Tools

A non-exhaustive list of free complementary tools:

GDB (http://www.gnu.org/software/gdb/)
 * Valgrind (http://valgrind.org/)
 * AddressSanitizer (http://code.google.com/p/address-sanitizer/wiki/AddressSanitizer)
 * strace (http://sourceforge.net/projects/strace/)
 * tcpflow (http://www.circlemud.org/~jelson/software/tcpflow/)

A non-exhaustive list of related free tools:
 * American fuzzy lop (http://lcamtuf.coredump.cx/afl/)
 * Zzuf (http://caca.zoy.org/wiki/zzuf)
 * Bunny the Fuzzer (http://code.google.com/p/bunny-the-fuzzer/)
 * Peach (http://peachfuzzer.com/)
 * Sulley (http://code.google.com/p/sulley/)

Tools which are intended to improve security are usually complementary and should be used in parallel to improve the results. Radamsa aims to be an easy-to-set-up general purpose shotgun test to expose the easiest (and often severe due to being reachable from via input streams) cracks which might be exploitable by getting the program to process malicious data. It has also turned out to be useful for catching regressions when combined with continuous automatic testing.

## Some Known Results

A robustness testing tool is obviously only good only if it really can find non-trivial issues in real-world programs. Being a University-based group, we have tried to formulate some more scientific approaches to define what a 'good fuzzer' is, but real users are more likely to be interested in whether a tool has found something useful. We do not have anyone at OUSPG running tests or even developing Radamsa full-time, but we obviously do make occasional test-runs, both to assess the usefulness of the tool, and to help improve robustness of the target programs. For the test-runs we try to select programs that are mature, useful to us, widely used, preferably open source and/or tend to process data from outside sources.

The list below has some CVE:s we know of that have been found by using Radamsa. Some of the results are from our own test runs, and some have been kindly provided by CERT-FI from their tests and other users. As usual, please note that CVE:s should be read as 'product X is now more robust (against Y)'.

CVE           | program    | credit
--------------|------------|-----------
CVE-2007-3641 | libarchive | OUSPG
CVE-2007-3644 | libarchive | OUSPG
CVE-2007-3645 | libarchive | OUSPG
CVE-2008-1372 | bzip2 | OUSPG
CVE-2008-1387 | ClamAV | OUSPG
CVE-2008-1412 | F-Secure | OUSPG
CVE-2008-1837 | ClamAV | OUSPG
CVE-2008-6536 | 7-zip | OUSPG
CVE-2008-6903 | Sophos Anti-Virus | OUSPG
CVE-2010-0001 | Gzip | integer underflow in unlzw | OUSPG
CVE-2010-0192 | Acroread | OUSPG
CVE-2010-1205 | libpng | OUSPG
CVE-2010-1410 | Webkit | OUSPG
CVE-2010-1415 | Webkit | OUSPG
CVE-2010-1793 | Webkit | OUSPG
CVE-2010-2065 | libtiff | found by CERT-FI
CVE-2010-2443 | libtiff | found by CERT-FI
CVE-2010-2597 | libtiff | found by CERT-FI
CVE-2010-2482 | libtiff | found by CERT-FI
CVE-2011-0522 | VLC | found by Harry Sintonen
CVE-2011-0181 | Apple ImageIO | found by Harry Sintonen
CVE-2011-0198 | Apple Type Services | found by Harry Sintonen
CVE-2011-0205 | Apple ImageIO | found by Harry Sintonen
CVE-2011-0201 | Apple CoreFoundation | found by Harry Sintonen
CVE-2011-1276 | Excel | found by Nicolas Grégoire of Agarri
CVE-2011-1186 | Chrome | OUSPG
CVE-2011-1434 | Chrome | OUSPG
CVE-2011-2348 | Chrome | OUSPG
CVE-2011-2804 | Chrome/pdf | OUSPG
CVE-2011-2830 | Chrome/pdf | OUSPG
CVE-2011-2839 | Chrome/pdf | OUSPG
CVE-2011-2861 | Chrome/pdf | OUSPG
CVE-2011-3146 | librsvg | found by Sauli Pahlman
CVE-2011-3654 | Mozilla Firefox | OUSPG
CVE-2011-3892 | Theora | OUSPG
CVE-2011-3893 | Chrome | OUSPG
CVE-2011-3895 | FFmpeg | OUSPG
CVE-2011-3957 | Chrome | OUSPG
CVE-2011-3959 | Chrome | OUSPG
CVE-2011-3960 | Chrome | OUSPG
CVE-2011-3962 | Chrome | OUSPG
CVE-2011-3966 | Chrome | OUSPG
CVE-2011-3970 | libxslt | OUSPG
CVE-2012-0449 | Firefox | found by Nicolas Grégoire of Agarri
CVE-2012-0469 | Mozilla Firefox | OUSPG
CVE-2012-0470 | Mozilla Firefox | OUSPG
CVE-2012-0457 | Mozilla Firefox | OUSPG
CVE-2012-2825 | libxslt | found by Nicolas Grégoire of Agarri
CVE-2012-2849 | Chrome/GIF | OUSPG
CVE-2012-3972 | Mozilla Firefox | found by Nicolas Grégoire of Agarri
CVE-2012-1525 | Acrobat Reader | found by Nicolas Grégoire of Agarri
CVE-2012-2871 | libxslt | found by Nicolas Grégoire of Agarri
CVE-2012-2870 | libxslt | found by Nicolas Grégoire of Agarri
CVE-2012-2870 | libxslt | found by Nicolas Grégoire of Agarri
CVE-2012-4922 | tor | found by the Tor project
CVE-2012-5108 | Chrome | OUSPG via NodeFuzz
CVE-2012-2887 | Chrome | OUSPG via NodeFuzz
CVE-2012-5120 | Chrome | OUSPG via NodeFuzz
CVE-2012-5121 | Chrome | OUSPG via NodeFuzz
CVE-2012-5145 | Chrome | OUSPG via NodeFuzz
CVE-2012-4186 | Mozilla Firefox | OUSPG via NodeFuzz
CVE-2012-4187 | Mozilla Firefox | OUSPG via NodeFuzz
CVE-2012-4188 | Mozilla Firefox | OUSPG via NodeFuzz
CVE-2012-4202 | Mozilla Firefox | OUSPG via NodeFuzz
CVE-2013-0744 | Mozilla Firefox | OUSPG via NodeFuzz
CVE-2013-1691 | Mozilla Firefox | OUSPG
CVE-2013-1708 | Mozilla Firefox | OUSPG
CVE-2013-4082 | Wireshark | found by cons0ul
CVE-2013-1732 | Mozilla Firefox | OUSPG
CVE-2014-3669 | PHP
CVE-2014-3668 | PHP
CVE-2014-3707 | cURL | Symeon Paraschoudis
CVE-2014-7933 | Chrome | OUSPG
CVE-2015-0797 | Mozilla Firefox | OUSPG
CVE-2015-0813 | Mozilla Firefox | OUSPG
CVE-2015-1220 | Chrome | OUSPG
CVE-2015-1224 | Chrome | OUSPG


We would like to thank the Chromium project and Mozilla for analyzing, fixing and reporting further many of the above mentioned issues, CERT-FI for feedback and disclosure handling, and other users, projects and vendors who have responsibly taken care of uncovered bugs.

## Troubleshooting

Issues in Radamsa can be reported to the issue tracker. The tool is under development, but we are glad to get error reports even for known issues to make sure they are not forgotten.

You can also drop by at #radamsa on Freenode if you have questions or feedback.

Issues your programs should be fixed. If Radamsa finds them quickly (say, in an hour or a day) chances are that others will too.

Issues in other programs written by others should be dealt with responsibly. Even fairly simple errors can turn out to be exploitable, especially in programs written in low-level languages. In case you find something potentially severe, like an easily reproducible crash, and are unsure what to do with it, ask the vendor or project members, or your local CERT.

# FAQ

Q: If I find a bug with radamsa, do I have to mention the tool?  
A: No.

Q: Will you make a graphical version of radamsa?  
A: No. The intention is to keep it simple and scriptable for use in automated regression tests and continuous testing.

Q: I can't install! I don't have root access on the machine!  
A: You can omit the $ make install part and just run radamsa from bin/radamsa in the build directory, or copy it somewhere else and use from there.

Q: Radamsa takes several GB of memory to compile!1  
A: This is most likely due to an issue with your C compiler. Use prebuilt images or try the quick build instrucitons in this page.

Q: Radamsa does not compile using the instructions in this page!  
A: Please file an issue at https://github.com/aoh/radamsa/issues if you don't see a similar one already filed, or contact us via email (ouspg@ee.oulu.fi) or IRC (#radamsa on freenode).

Q: I used fuzzer X and found much more bugs from program Y than Radamsa did.  
A: Cool. Let us know about it (ouspg@ee.oulu.fi) and we'll try to hack something X-ish to radamsa if it's general purpose enough. It'd also be useful to get some samples which you used to check how well radamsa does, because it might be overfitting some heuristic.

Q: Can I get support for using radamsa?  
A: You can send email to us at ouspg@ee.oulu.fi or check if some of us happen to be hanging around at #radamsa on freenode.

Q: Can I use radamsa on Windows?  
A: An experimental Windows executable is now in Downloads, but we have usually not tested it properly since we rarely use Windows internally. Feel free to file an issue if something is broken.

Q: How can I install radamsa?  
A: Grab a binary from downloads and run it, or $ make && sudo make install.

Q: How can I uninstall radamsa?  
A: Remove the binary you grabbed from downloads, or $ sudo make uninstall.

Q: Why are many outputs generated by Radamsa equal?  
A: Radamsa doesn't keep track which outputs it has already generated, but instead relies on varying mutations to keep the output varying enough. Outputs can often be the same if you give a few small samples and generate lots of outputs from them. If you do spot a case where lots of equal outputs are generated, we'd be interested in hearing about it.

Q: There are lots of command line options. Which should I use for best results?  
A: The recommended use is $ radamsa -o output-%n.foo -n 100 samples/*.foo, which is also what is used internally at OUSPG. It's usually best and most future proof to let radamsa decide the details.

Q: How can I make radamsa faster?  
A: Radamsa typically writes a few megabytes of output per second. If you enable only simple mutations, e.g. -m bf,bd,bi,br,bp,bei,bed,ber,sr,sd, you will get about 10x faster output.

Q: What's with the funny name?  
A: It's from a scene in a Finnish children's story. You've probably never heard about it.

Q: Is this the last question?  
A: Yes.

## Warnings

Use of data generated by radamsa, especially when targeting buggy programs running with high privileges, can result in arbitrarily bad things to happen. A typical unexpected issue is caused by a file manager, automatic indexer or antivirus scanner trying to do something to fuzzed data before they are being tested intentionally. We have seen spontaneous reboots, system hangs, file system curruption, loss of data and other nastiness. When in doubt, use a disposable system, throwaway profile, chroot jail, sandbox, separate user account or an emulator.

Not safe when used as prescribed.

This product may contain faint traces of parenthesis.
