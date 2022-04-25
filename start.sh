#!/bin/sh
# Entry point
cd "$( dirname "$0" )" || exit

# PATH setup
export PATH="$HOME/.cabal/bin":"$HOME/.ghcup/bin":"$PATH"

# TODO Ubuntu notification setup

# Config ibus
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT4_IM_MODULE=ibus
export QT_IM_MODULE=ibus

# GDK scales for HDPI
export GDK_SCALE=2
export GDK_DPI_SCALE=0.5
export QT_AUTO_SCREEN_SCALE_FACTOR=1

exec xmonad-manage run "$1" > "./logs/common.log" 2> "./logs/common.err"
