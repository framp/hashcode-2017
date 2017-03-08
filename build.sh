#!/bin/sh
ghc -O2 -isrc --make -threaded -rtsopts -o hashcode src/Main.hs
