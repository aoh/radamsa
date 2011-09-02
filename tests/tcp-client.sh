#!/bin/sh

fail() {
   echo "ERROR - " $@
   exit 1
}

echo -n "("

$@ -o 127.0.0.1:31338 -n 5 *.l > /dev/null & 

for foo in 1 2 3 4 5
do
   echo -n "-"
   nc -l -p 31338 > /dev/null || fail "didn't get file number $foo"
done

echo -n ")"

kill -9 %1 2>/dev/null && fail "radamsa was running after the files"

true
