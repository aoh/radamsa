#!/bin/sh

echo "foo" | $@ -m nop -p od | grep -q "^foo$" || exit 1
