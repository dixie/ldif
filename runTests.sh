#!/bin/sh

function showUsage()
{
   echo "Usage: $1 [ build | rebuild | run ]"
}

if [ -z "$1" ]; then
   showUsage $0
   exit 1;
fi

if [ "$1" = "run" ]; then
    ./dist/build/test/test
elif [ "$1" = "build" ]; then
    cabal configure -ftest && cabal build && ./dist/build/test/test
elif [ "$1" = "rebuild" ]; then
    cabal clean
    cabal configure -ftest && cabal build && ./dist/build/test/test
else
    showUsage $0
fi
