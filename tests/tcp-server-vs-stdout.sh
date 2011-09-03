#!/bin/bash

fail() {
   echo "ERROR - " $@
   exit 1
}

mkdir -p tmp

NFILES=100
SEED=$RANDOM

$@ -o - --seed $SEED -n $NFILES *.l > tmp/stdout-$$

$@ -o :31337 --seed $SEED -n $NFILES *.l &

echo -n "" > tmp/tcp-$$

echo -n "("

## read the connections using owl, which is already a dependency and doesn't seem to 
## misbehave occasionally as netcat did (not sure why)

echo "
(define (stderr-data fd)
   (let ((block (interact fd 'input)))
      (if (and block (not (eof? block)))
         (begin
            (mail stderr block)
            (stderr-data fd)))))
(let loop ((n 0))
   (if (< n $NFILES)
      (let ((fd (open-connection (vector 127 0 0 1) 31337)))
         (if fd
            (begin
               (mail stdout 45)
               (flush-port stdout)
               (stderr-data fd)
               (loop (+ n 1)))
            (loop n)))))
" | ol -q 2> tmp/tcp-$$

diff -q tmp/stdout-$$ tmp/tcp-$$ || fail "tcp server output differs from stdout output"

echo -n ") "

jobs &> /dev/null # background job will get wait()ed here and should no longer show up

pkill -9 %1 2>&1 && fail "Radamsa was left running"

rm tmp/*$$

true
