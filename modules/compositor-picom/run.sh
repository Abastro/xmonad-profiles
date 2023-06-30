#!/bin/sh
picom -bcCGf -D 4 -I 0.02 -O 0.02 -e 0.8

# Workaround for dbus being displayed first
dbus-update-activation-environment QT_AUTO_SCREEN_SCALE_FACTOR
