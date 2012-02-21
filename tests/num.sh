#!/bin/sh

echo " 100 + 100 + 100 " | $@ -m num -n 500 -p od | grep -q " 101 " || exit 1

