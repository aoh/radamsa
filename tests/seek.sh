#!/bin/bash

set -e

SEED=$RANDOM

echo "HAL 9000" | $@ --seed $SEED -o tmp/seek-1-%n -n 20
echo "HAL 9000" | $@ --seed $SEED -o tmp/seek-2-%n --seek 19 -n 2

echo "HAL 9000" | $@ --seed $SEED -o tmp/seek-2-%n --seek 10

cmp tmp/seek-1-10 tmp/seek-2-10 || exit 1
cmp tmp/seek-1-19 tmp/seek-2-19 || exit 1
cmp tmp/seek-1-20 tmp/seek-2-20 || exit 1

test -f tmp/seek-2-18 && exit 2
test -f tmp/seek-2-21 && exit 2

rm tmp/seek-*
