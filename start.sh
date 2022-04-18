#!/bin/sh
# Entry point
cd "$( dirname "$0" )" || exit
exec xmonad-manage run "$1"
