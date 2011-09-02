#!/bin/bash

NFILES=10

fail() {
   echo "ERROR - " $@
   exit 1
}

echo -n "("
for round in $(ol -e '(iota 0 1 20)')
do
   echo -n "-"
   SEED=$RANDOM # bashism
   FIRST=`$@ -n $NFILES -o - -s $SEED *.l | md5sum`
   SECOND=`$@ -n $NFILES -o - -s $SEED *.l | md5sum`

   test "$FIRST" = "$SECOND" || fail "sums differ $FIRST $SECOND seed $SEED"
done
echo -n ") "
