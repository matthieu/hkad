#!/usr/bin/env sh
mkdir -p build
ghc -o trim -threaded -isrc -O2 --make -odir build -hidir build src/*.hs

