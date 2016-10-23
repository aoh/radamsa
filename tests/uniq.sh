#!/bin/bash

set -e
echo "HAL 9000" | $@ -o tmp/uniq-%n -n 100 -p od -m num -u
test 0 = $(md5sum tmp/uniq-* | sed -e 's/ .*//' | sort | uniq -c | grep -v " 1 " | wc -l)
rm tmp/uniq-*
