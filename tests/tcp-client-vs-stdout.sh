#!/bin/bash

fail() {
   echo "ERROR - " $@
   exit 1
}

mkdir -p tmp

NFILES=100
SEED=$RANDOM

$@ -o - --seed $SEED -n $NFILES *.l > tmp/stdout-$$

# check that we did get something
test -s tmp/stdout-$$ || fail "didn't make anything"

# this will retry connections until successful, so must be ok to start before listening
$@ -o 127.0.0.1:31337 --seed $SEED -n $NFILES *.l &

echo -n "" > tmp/tcp-$$

echo -n "("

#for foo in $(ol -e "(iota 0 1 $NFILES)")
#do
#   echo -n "-"
#   nc -l -p 31337 >> tmp/tcp-$$
#done

echo "
(define (stderr-data fd)
   (let ((block (interact fd 'input)))
      (if (and block (not (eof? block)))
         (begin
            (mail stderr block)
            (stderr-data fd)))))
(define sock (open-socket 31337))
(let loop ((n 0))
   (if (< n $NFILES)
      (let ((fd (interact sock 'accept)))
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
