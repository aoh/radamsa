#!/bin/sh

# check bad string insertion happens as intended (more likely within quoted area)

echo '-----------------------------------------------------------------""---------------------------------------------------------------------------' \
   | $@ -m ab -p od -n 20 | grep -q '^-*\".*%.*\"-*$' || exit 1

