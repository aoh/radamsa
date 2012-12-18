#!/bin/sh

echo foo | $@ -M - -o /dev/null -p od -m bi 2>&1 | grep -q 'byte-insert: 1, generator: stdin, nth: 1, path: "/dev/null", output: file-writer, length: 5, pattern: once-dec'

