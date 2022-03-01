#!/bin/bash

cabal run :program experimenting/tests/Common.hs
ghc -o experimenting/output experimenting/Output.hs

