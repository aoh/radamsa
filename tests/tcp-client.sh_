#!/bin/sh

$@ -o 127.0.0.1:31337 -n 10 --seed 42 tests/*  &

PID=$!

sleep 0.3

kill -0 $PID || exit 1

for foo in $(seq 10)
do
   nc -l -p 31337 || break
done | md5sum > tmp/tcp-a

$@ -o - -n 10 --seed 42 tests/* | md5sum > tmp/tcp-b

kill -0 $PID 2>/dev/null && { kill -9 $PID; exit 1; }

cmp tmp/tcp-a tmp/tcp-b || exit 1
