#!/bin/bash

fail() {
   echo "ERROR - " $@
   exit 1
}

echo "level" > tmp/test-$$
$@ -n 10 tmp/test-$$ \
   > tmp/test-$$-stdout \
  2> tmp/test-$$-stderr 

test -s tmp/test-$$-stdout || fail "nothing at stdout"
test -s tmp/test-$$-stderr && fail "something at stderr"

true
