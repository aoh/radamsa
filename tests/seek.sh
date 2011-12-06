#!/bin/sh

# TODO: one would think runa > foo; runb >> foo; runc >> foo works when 
# making the files separately, but apparently it doesn't. examine later!

fail() {
   echo "ERROR - " $@
   exit 1
}

SAMPLES=*.l

test -d tmp || mkdir tmp

echo -n "("
for round in $(ol -e "(iota 0 1 10)")
do
   echo -n "-"
   SEED=`cat -v /dev/urandom | head -n 1 | sed -e 's/[^0-9]//g' -e 's/^/1/'`
   $@ -n 3 -s $SEED $SAMPLES > tmp/abc # make 3 at once
   $@ -S 0 -s $SEED $SAMPLES > tmp/a # make 1 at offset 0
   $@ -S 1 -s $SEED $SAMPLES > tmp/b # make 1 at offset 1
   $@ -S 2 -s $SEED $SAMPLES > tmp/c # make 1 at offset 2
   cat tmp/a tmp/b tmp/c > tmp/all
   diff -q tmp/abc tmp/all || fail "FILES DIFFER: tmp/ab tmp/both for seed $SEED"
   rm tmp/abc tmp/all tmp/[abc]
done

echo -n ") "
