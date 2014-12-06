#!/bin/sh

# check that xml node duplication (or repetition) works

echo 'x <foo>bar</foo> y' | $@ -m xp -p od -n 100 | grep -q 'x <foo>bar</foo><foo>bar</foo> y' || exit 1
