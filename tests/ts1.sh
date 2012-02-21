#!/bin/sh

echo 'A (a) (b) (c) B' | $@ -m ts1 -p od -n 100 | sort | uniq | wc -l | grep -q 6 || exit 1 

