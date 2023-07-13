#!/bin/sh
parent=$( dirname "$0" )

# Configurations for XSettings.
cp -T "$parent/xsettingsd.conf" "$HOME/.config/xsettingsd/xsettingsd.conf"

# Copy X resources to HOME.
cp -T "$parent/.Xresources" "$HOME/.Xresources"
