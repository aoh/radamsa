#!/bin/sh

echo -e "foo\nbar\nbaz" | $@ -p od -m ld | wc -l | grep -q 2 || exit 1

