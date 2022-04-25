#!/bin/sh
# commons=$( dirname "$0" )

# Monitor settings use xrandr
xrandr

# Load X resources
xrdb -merge ~/.Xresources

# To support transparency
# xcompmgr -n &

# * picom is not available in 20.04 ubuntu repo. Use it if you could install.
picom -bcCGf -D 4 -I 0.02 -O 0.02 -e 0.8

# Starts ibus-daemon
ibus-daemon -drx &

# Starts gnome-screensaver
gnome-screensaver &

# Starts xss-lock
xss-lock -- gnome-screensaver -l &

# Keyring-daemon for keys
gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh


# Set root cursor
xsetroot -cursor_name left_ptr
