#!/bin/sh
cd "$( dirname "$0" )" || exit
# Statically built for consistency
cabal install "exe:xmonad-manage" --overwrite-policy=always --disable-executable-dynamic
echo ""
xmonad-manage
