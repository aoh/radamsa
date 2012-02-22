#!/bin/sh

echo -e "foo\nbar" | $@ -m ls -p od | head -n 1 | grep -q bar || exit 1

