#!/bin/sh

# blank sample files should (with very high probability) trigger random data to be generated
touch tmp/blank
$@ < tmp/blank > tmp/nonblank
cmp -s tmp/blank tmp/nonblank && exit 1
rm tmp/blank tmp/nonblank

