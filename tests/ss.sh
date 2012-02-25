#!/bin/sh

echo zombieslartibartfasterthaneelslartibartfastenyourseatbelts | $@ -m ss -p od -n 30 | grep -q zombieslartibartfastenyourseatbelts || exit 1

