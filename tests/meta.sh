#!/bin/sh

echo foo | $@ -M - -o /dev/null -p od -m bi 2>&1 | grep -q 'byte-insert: 1'

