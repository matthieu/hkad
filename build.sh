#!/usr/bin/env sh
mkdir -p build
ghc -threaded -isrc -O2 --make -odir build -hidir build src/*.hs

