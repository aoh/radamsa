#!/bin/sh

echo aa | $@ -p od -m sr | grep -q aaa || exit 1

