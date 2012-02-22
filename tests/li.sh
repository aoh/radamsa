#!/bin/sh

echo -e "a\n a\n  a\n   a\n    a" | $@ -p od -m li | wc -l | grep -q 6 || exit 1

