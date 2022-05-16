#!/bin/bash

set -e

mkdir -p comp-transform/build
cabal -v0 v2-run program comp-transform/Common.hs > comp-transform/build/Output.hs
ghc -icomp-transform/lib -o comp-transform/build/output comp-transform/build/Output.hs
comp-transform/build/output
