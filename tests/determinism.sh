#!/bin/sh

NFILES=10
SAMPLES=tests/*

fail() {
   echo "ERROR - " $@
   exit 1
}

echo -n "("
for round in $(ol -e '(iota 0 1 20)')
do
   echo -n "-"
   SEED=`cat -v /dev/urandom | head -n 1 | sed -e 's/[^0-9]//g' -e 's/^/1/'`
   FIRST=`$@ -n $NFILES -o - -s "$SEED" $SAMPLES > det-$$-1`
   SECOND=`$@ -n $NFILES -o - -s "$SEED" $SAMPLES > det-$$-2`

   cmp det-$$-1 det-$$-2 || fail "sums differ $FIRST $SECOND seed $SEED"
done
echo -n ") "

rm det-$$-*
