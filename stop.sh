#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR

kill `cat haskell_server.pid`
rm haskell_server.pid
