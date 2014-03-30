#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR

cabal clean
cabal install --only-dependencies
cabal configure
cabal build
dist/build/player/player 1900 &

echo $! > haskell_server.pid
