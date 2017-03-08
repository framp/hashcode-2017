#!/bin/sh
echo 'ghc -O2 -isrc --make -threaded -rtsopts -o hashcode src/Main.hs'
ghc -O2 -isrc --make -threaded -rtsopts -o hashcode src/Main.hs
echo 'rm src/*.o src/*.hi'
rm src/*.o src/*.hi