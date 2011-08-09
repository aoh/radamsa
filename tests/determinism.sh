#!/bin/bash

NFILES=10

fail() {
   echo "ERROR - " $@
   exit 1
}

for round in $(seq 1 1 20)
do
   echo -n " o"
   SEED=$RANDOM # bashism
   FIRST=`$@ -n $NFILES -o - -s $SEED fuz/*.* | md5sum`
   SECOND=`$@ -n $NFILES -o - -s $SEED fuz/*.* | md5sum`

   test "$FIRST" = "$SECOND" || fail "sums differ $FIRST $SECOND seed $SEED"
done
