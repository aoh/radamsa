#!/bin/bash

fail() {
   echo "ERROR - " $@
   exit 1
}

mkdir -p tmp

NFILES=70
SEED=$RANDOM

$@ -o - --seed $SEED -n $NFILES *.l > tmp/stdout-$$

$@ -o :31337 --seed $SEED -n $NFILES *.l &

echo -n "" > tmp/tcp-$$

echo -n "("

for foo in $(ol -e "(iota 0 1 $NFILES)")
do
   echo -n "-"
   nc localhost 31337 >> tmp/tcp-$$
done

diff -q tmp/stdout-$$ tmp/tcp-$$ || fail "tcp server output differs from stdout output"

echo -n ")"

rm tmp/*$$

true
