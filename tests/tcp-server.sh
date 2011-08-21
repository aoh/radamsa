#!/bin/sh

fail() {
   echo "ERROR - " $@
   exit 1
}


$@ -o :31337 -n 5 --tabula-rasa > /dev/null & 

sleep 0.5

for foo in 1 2 3 4 5
do
   echo -n " o"
   nc localhost 31337 > /dev/null || fail "didn't get file number $foo"
done

nc localhost 31337 >/dev/null 2>&1 && fail "got too many connections"

true
