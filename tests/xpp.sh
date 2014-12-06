#!/bin/sh

# check that xml node path copying happens

echo '<foo>bar</foo>' | $@ -m xp -p od -n 100 | grep -q '<foo><foo><foo>.*bar.*<\/foo><\/foo><\/foo>' || exit 1
