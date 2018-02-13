#!/bin/sh

echo '(a) (b)' | $@ -m tr2 -C 0 -p od -n 30 | sort 2>/dev/null | uniq | wc -l | grep -q 2 || exit 1

