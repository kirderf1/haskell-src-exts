#!/bin/bash

#dist-newstyle/build/x86_64-linux/ghc-8.10.1/haskell-src-exts-1.23.1/x/program/build/program/program experimenting/tests/Common.hs > experimenting/Output.hs
cabal -v0 run program experimenting/tests/Common.hs > experimenting/Output.hs
ghc -o experimenting/output experimenting/Output.hs
experimenting/output
