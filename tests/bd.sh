#!/bin/sh

/bin/echo -n ab | $@ -p od -m bd | grep -q "^[ab]$"

