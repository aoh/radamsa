#!/bin/bash

fail() {
   echo "ERROR - " $@
   exit 1
}

test -d tmp || mkdir tmp

for round in $(seq 1 1 2)
do
   echo -n " o"
   SEED=$RANDOM # bashism
   $@ -n 20 -o tmp/test-1-$$-%n -s $SEED *.*  # make 1-20, or 20 after --seek 0
   $@ -n 10 -o tmp/test-2-$$-%n -s $SEED *.*  # make 1-10
   $@ -n 10 --seek 10 -o tmp/test-2-$$-%n -s $SEED *.* # make 11-20 (10 after 10)
   FIRST=`cat tmp/test-1-$$* | md5sum`
   SECOND=`cat tmp/test-2-$$* | md5sum`
   test "$FIRST" = "$SECOND" || fail "sums differ $FIRST $SECOND seed $SEED"
done
