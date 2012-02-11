#!/bin/sh

echo -n " ("
for foo in $(seq 1 10)
do
   cat /dev/urandom | head -n 100 | $@ > /dev/null || exit 1
   echo -n "-"
done

echo -n ") "
