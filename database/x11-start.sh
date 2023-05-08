#!/bin/sh
parent=$( dirname "$0" )

# Monitor settings use xrandr
xrandr

# Copy X resources
cp -T "$parent/.Xresources" "$HOME/.Xresources"
# Load X resources
xrdb -merge ~/.Xresources

# Set root cursor
xsetroot -cursor_name left_ptr
