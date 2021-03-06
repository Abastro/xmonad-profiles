#!/bin/sh
parent=$( dirname "$0" )

echo "Startup for LightDM..."

# Monitor settings use xrandr
xrandr

# Copy X resources
cp -T "$parent/.Xresources" "$HOME/.Xresources"
# Load X resources
xrdb -merge ~/.Xresources

# picom does transparency
picom -bcCGf -D 4 -I 0.02 -O 0.02 -e 0.8

# ibus-daemon provides ibus input handling
ibus-daemon -drx &

# locker dedicated for lightdm
light-locker &

# TODO: Make this somehow a separate settings
xset s 3000 dpms 3600 3600 3600

# Keyring-daemon for e.g. ssh keys
gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh


# Set root cursor
xsetroot -cursor_name left_ptr

