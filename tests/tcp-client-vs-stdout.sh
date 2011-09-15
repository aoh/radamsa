#!/bin/bash

# check that what is sent to host/port over tcp matches what comes out of stdout

fail() {
   echo "ERROR - " $@
   exit 1
}

SAMPLES=tests/*
mkdir -p tmp

NFILES=10

echo -n "("
for foo in $(ol -e "(iota 0 1 $NFILES)")
do
   SEED=$RANDOM
   echo -n "-"
   $@ --seed $SEED $SAMPLES > tmp/stdout-$$
   $@ -o 127.0.0.1:31337 --seed $SEED $SAMPLES 2> tmp/radamsa-$$ & 
   echo "(mail stdout (force (port->byte-stream (interact (open-socket 31337) 'accept)))) (close-port stdout)" | ol -q > tmp/tcp-$$
   # not using netcat because there are minor changes in command line flags
   #strace nc -l -p 31337 > tmp/tcp-$$ 2>tmp/nc-$$                 # should be the same

   diff -q tmp/stdout-$$ tmp/tcp-$$ || fail "files differ: tmp/*-$$"
done

echo -n ") "

rm tmp/*-$$

true
