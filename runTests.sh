#!/usr/bin/env sh
exit 0
for f in `find Examples/ -name '*.hs'`; do
    touch $f;
    sp ghc -v0 -isrc --make -c $f || exit 1;
    echo -n .;
 done
echo Done