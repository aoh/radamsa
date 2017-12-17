#!/bin/bash

set -e

$@ -H sha256 -M tmp/out.meta -o tmp/out rad/*

# sha256sum command may be missing or called sha256 on soma machines, so use owl
SHA=$(bin/ol -e '(sha256 (file->list "tmp/out"))')

grep -q "$SHA" tmp/out.meta

