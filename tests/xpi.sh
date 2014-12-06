#!/bin/sh

# check that xml node insertion learns attributes 

echo '<foo a1=v1> hai' > tmp/xpi-$$-1
echo '<foo a2=v2> hoi' > tmp/xpi-$$-2

$@ -n 100 -m xp -p od tmp/xpi-$$-* | grep -q '<foo a.=v. a.=v.>' || exit 1

rm tmp/xpi-$$-*
