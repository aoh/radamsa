#!/bin/sh

$@ -o :31337 -n 10 --seed 42 tests/*  &
sleep 0.3

while true
do
   nc localhost 31337 < /dev/null || break
done | md5sum > tmp/tcp-a
$@ -o - -n 10 --seed 42 tests/* | md5sum > tmp/tcp-b

cmp tmp/tcp-a tmp/tcp-b || exit 1
