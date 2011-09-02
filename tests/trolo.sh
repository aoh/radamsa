#!/bin/sh

echo -n "("
while true
do
   echo -n "-"
   echo "trol" | $@ | grep -q trolololo && break
done

echo -n ") "
