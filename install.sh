#!/bin/sh
cd "$( dirname "$0" )" || exit
cabal install "exe:xmonad-manage" --overwrite-policy=always
