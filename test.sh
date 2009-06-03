#!/usr/bin/env sh
HPCTIXDIR=build
ghc -fhpc -threaded --make -odir build -hidir build src/*.hs
ghc -fhpc -threaded -o build/Test -odir build -hidir build src/*.hs test/Test.hs --make
./build/Test +RTS -N2
hpc markup Test --exclude=Main --exclude=QC
mv *.html build
mv *.tix build
