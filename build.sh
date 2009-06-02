#!/usr/bin/env sh
mkdir -p build
ghc -O2 --make -odir build -hidir build src/*.hs

