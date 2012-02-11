#!/bin/sh

echo -n " ("
for foo in $(seq 1 10)
do
   $@ < radamsa.l > /dev/null || exit 1
   echo -n "-"
done

echo -n ") "
