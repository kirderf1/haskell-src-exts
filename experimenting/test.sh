#!/bin/bash

cabal -v0 run program experimenting/tests/Common.hs > experimenting/build/Output.hs
ghc -o experimenting/build/output experimenting/build/Output.hs
experimenting/build/output
