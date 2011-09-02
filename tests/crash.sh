#!/bin/sh

$@ -f crash 2>&1 > /dev/null
test $? = 127 || exit 1

true

