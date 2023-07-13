#!/bin/sh
echo "Running X11-XMonad setup..."

# Need to reflect to the Dbus environment.
dbus-update-activation-environment GTK_THEME QT_AUTO_SCREEN_SCALE_FACTOR QT_QPA_PLATFORMTHEME

# Set up XSettings.
xsettingsd &

# Monitor settings use xrandr
xrandr

# Load X resources from HOME.
xrdb -merge "$HOME/.Xresources"

# Set root cursor.
xsetroot -cursor_name left_ptr
