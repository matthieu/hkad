#!/usr/bin/env sh
HPCTIXDIR=build
ghc -fhpc --make -odir build -hidir build src/*.hs
ghc -fhpc -o build/Test -odir build -hidir build src/*.hs test/Test.hs --make
./build/Test
hpc markup Test --exclude=Main --exclude=QC
mv *.html build
mv *.tix build
