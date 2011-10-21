#!/bin/sh

echo -n "("
while true
do
   echo -n "-"
   echo "254 + 256 + 1 + 2" | $@ -t num | grep -q 255 && break
done

echo -n ") "
