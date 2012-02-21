#!/bin/sh

echo '(a) (b)' | $@ -m tr2 -p od -n 30 | sort | uniq | wc -l | grep -q 2 || exit 1

