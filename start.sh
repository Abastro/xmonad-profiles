#!/bin/sh
# Entry point
cd "$( dirname "$0" )" || exit

# PATH setup
export PATH="$HOME/.cabal/bin":"$HOME/.ghcup/bin":"$PATH"

# TODO Ubuntu notification setup

exec xmonad-manage run "$1" > "./logs/common.log" 2> "./logs/common.err"
