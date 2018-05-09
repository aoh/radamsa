#!/bin/sh

PORT=9002
SEED=$(bin/ol -e '(time-ms)')

test -f tmp/udp && rm tmp/udp

# receive a single udp packet to tmp/udp
bin/ol -t "(vector->file (cdr (lcar (udp-packets $PORT))) \"tmp/udp\")" &

sleep 0.3

$@ -n 1 --seed $SEED -o 127.0.0.1:$PORT/udp tests/*

# could check prefix with the same seed
test -f tmp/udp
