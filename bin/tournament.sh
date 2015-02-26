#!/bin/bash

THREADS=1
SETSIZE=100
TIMEOUT=10
ARENA=$(pwd)/tournament
FLAGS=""

USAGE="Usage: tournament --arena <test-dir> --timeout <timeout($TIMEOUT)> --setsize <setsize($SETSIZE)> -j <njobs($THREADS)> -m_foo_-p_bar ..."

test $# = 0 && echo "$USAGE" && exit 0

fail() {
   echo "FAIL: $@"
   exit 1;
}

waitnjobs() {
   while true
   do
      test $(jobs | grep Running | wc -l) -lt $1 && break
      sleep 0.2
   done
}

while [ $# != 0 ]
do
   case "$1" in
      (--help|-h)
         echo "$USAGE"
         exit 0;;
      (--arena|-a)
         test $# == 1 && fail "$1 needs an argument"
         ARENA=$(readlink -f $2)
         test -d $ARENA || fail "$ARENA is not there"
         shift 2;;
      (--timeout|-t)
         test $# == 1 && fail "$1 needs an argument"
         TIMEOUT=$2
         shift 2;;
      (--jobs|-j)
         test $# == 1 && fail "$1 needs an argument"
         THREADS=$2
         shift 2;;
      (--setsize|-n)
         test $# == 1 && fail "$1 needs an argument"
         SETSIZE=$2
         shift 2;;
      (*)
         FLAGS="$@"
         break
         # what this flag?
   esac
done

test -n "$FLAGS" || fail "No flags to compare"

echo "$THREADS threads"
echo "set size $SETSIZE"
echo "timeout $TIMEOUT"
echo "arena is $ARENA"
echo "flags:" 
for foo in $FLAGS do; echo " - $foo" | sed -e 's/_/ /g' done

ls $ARENA | grep -q "target-" || fail "no $ARENA/target-* found"

for TARGET in $ARENA/target-*
do
   echo Competing at $TARGET
   cd $TARGET || exit 1
   for ARGS in $FLAGS
   do
      CMD=$(echo "radamsa $ARGS" | sed -e 's/_/ /g')
      echo "   using $CMD"
   done
done



