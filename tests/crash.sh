#!/bin/sh

$@ -t crash < radamsa.l > /dev/null 2>&1
test $? = 127 || exit 1

true

