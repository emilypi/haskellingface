#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
cabal check
cabal build all --enable-tests --ghc-options=-Werror
cabal test all --test-show-details=direct --ghc-options=-Werror
cabal haddock all
cabal sdist
