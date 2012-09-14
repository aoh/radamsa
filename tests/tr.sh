#!/bin/sh

echo "(x (Y x))" | $@ -p od -m tr -n 50 | grep -q "(x (x (x (x (Y x)))))" || exit 1

