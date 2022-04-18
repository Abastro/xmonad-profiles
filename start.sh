#!/bin/sh
# Entry point
cd "$( dirname "$0" )" || exit

# PATH setup
export PATH="$HOME/.cabal/bin":"$HOME/.ghcup/bin":"$PATH"

# TODO Move this out to personal config thing
# For GTK2
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# Config ibus
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT4_IM_MODULE=ibus
export QT_IM_MODULE=ibus

# GDK scales for HDPI
GDK_SCALE=2
GDK_DPI_SCALE=0.5
QT_AUTO_SCREEN_SCALE_FACTOR=1
export GDK_SCALE GDK_DPI_SCALE QT_AUTO_SCREEN_SCALE_FACTOR

exec xmonad-manage run "$1" > "./logs/common.log" 2> "./logs/common.err"
