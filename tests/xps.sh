#!/bin/sh

# check that xml node swapping works

echo 'X <foo>A</foo> Y <bar>B</bar> Z' | $@ -m xp -n 100 | grep -q 'X <bar>B<\/bar> Y <foo>A<\/foo> Z' || exit 1

