#!/bin/sh

SAMPLES=$@
NSAMPLES=$#
NGEN=1000

NTOTAL=0
NUNIQ=0
THREADS=3

radamsa --version | grep -i radamsa || exit 1
ol --version | grep "Owl Lisp" || exit 2

echo "Uniqueness test: $NSAMPLES files with $(cat $SAMPLES | wc -c) bytes"

mkdir uniq-$$ && cd uniq-$$ || exit 1

touch seen

while true
do
   for foo in $(seq $THREADS)
   do
      radamsa -o rad-$foo-%n -n $NGEN $SAMPLES &
   done

   wait

   NTOTAL=$(ol -e "(+ $NTOTAL (* $NGEN $THREADS))")
   mv seen prev
   (cat prev; md5sum rad-* | sed -e 's/ .*//') | sort | uniq > seen
   NUNIQ=$(wc -l < seen);
   PERC=$(ol -e "(round (/ (* 100 $NUNIQ) $NTOTAL))")
   echo " - ${PERC}% unique at $NTOTAL"
done 
