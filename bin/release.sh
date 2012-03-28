#!/bin/bash

set -e

make

DIR=`bin/radamsa --version | tr '[A-Z]' '[a-z]' | sed -e 's/ /-/g'`

echo "Target is $DIR"

test -d $DIR && rm -rf $DIR

mkdir $DIR
cp -va tests rad doc Makefile radamsa.c readme.txt $DIR
tar -f - -c $DIR | gzip -9 > $DIR.tar.gz
cd $DIR
make && echo target built ok && cd .. && gpg -b $DIR.tar.gz
