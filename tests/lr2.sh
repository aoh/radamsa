#!/bin/sh

echo -e "foo\nbar\nbaz" | $@ -p od -m lr2 | wc -l | grep -q 4 || exit 1

