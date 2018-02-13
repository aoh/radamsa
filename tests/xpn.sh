#!/bin/sh

# check that xml attributes get likely desired number values occasionally

echo '<foo bar=42><baz quux=42>' | $@ -C 0 -m xp -p od -n 2000 | grep -q 6553 || exit 1

