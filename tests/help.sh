#!/bin/bash

fail() {
   echo "ERROR - " $@
   exit 1
}

$@ --help | grep -q Usage || fail "no usage in stdout"
