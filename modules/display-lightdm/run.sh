#!/bin/sh
# locker dedicated for lightdm
light-locker &

# TODO: Make this somehow a separate settings
xset s 3000 dpms 3600 3600 3600
