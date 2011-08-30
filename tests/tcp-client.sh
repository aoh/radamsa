#!/bin/sh

fail() {
   echo "ERROR - " $@
   exit 1
}

# send 5 somethings 
$@ -o 127.0.0.1:31338 -n 5 *.l > /dev/null & 

for foo in 1 2 3 4 5
do
   echo -n " o"
   nc -l -p 31338 > /dev/null || fail "didn't get file number $foo"
done

sleep 0.5 # make sure radamsa has exited

kill -9 %1 2>/dev/null && fail "radamsa was running after the files"

true
