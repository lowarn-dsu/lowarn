#!/bin/bash

SCRIPT=$(readlink -f "$0")

cd "$(dirname "$SCRIPT")"
find . -type f -name "*.coverables" -exec rm {} \;
find . -type f -name "coverage*.dat" -exec rm {} \;
cabal build all -f dekking
LOWARN_PACKAGE_ENV=$(realpath .ghc.environment*) cabal test all -f dekking
cabal-docspec
find . -type f -name "coverage*.dat" | sed -e 's/^/--coverage=/' | xargs dekking-report --coverables . --output coverage
