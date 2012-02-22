#!/bin/sh

echo zombieslartibartfasterthaneelslartibartfastenyourseatbelts | $@ -m ss -p od -n 10 | grep -q zombieslartibartfastenyourseatbelts || exit 1

