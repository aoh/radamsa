#!/bin/sh

echo "(x (Y x))" | $@ -p od -m tr -n 20 | grep -q "(x (x (x (x (Y x)))))" || exit 1

