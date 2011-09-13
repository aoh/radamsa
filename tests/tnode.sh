#!/bin/sh

echo -n "("
while true
do
   echo -n "-"
   echo "K (S K) (S K K)" | bin/radamsa | grep -q "K (S K K) (S K)" && break
done

echo -n ") "
