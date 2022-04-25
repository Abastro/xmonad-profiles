#!/bin/sh
parent=$( dirname "$0" )

# Monitor settings use xrandr
xrandr

# Copy X resources
cp -T "$parent/.Xresources" "$HOME/.Xresources"
# Load X resources
xrdb -merge ~/.Xresources

# To support transparency
xcompmgr -n &

# Starts ibus-daemon
ibus-daemon -drx &

# Starts xscreensaver
xscreensaver -no-splash &

# Starts xss-lock
# TODO Move to using DM's solution
xss-lock -- xscreensaver-command -lock &

# Keyring-daemon for keys
gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh


# Set root cursor
xsetroot -cursor_name left_ptr

