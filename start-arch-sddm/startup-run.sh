#!/bin/sh
echo "Startup for SDDM..."

# picom does transparency
picom -bcCGf -D 4 -I 0.02 -O 0.02 -e 0.8

# ibus-daemon provides ibus input handling
ibus-daemon -drx &

# SDDM locker?

# TODO: Make this somehow a separate settings
# xset s 3000 dpms 3600 3600 3600

# KDE Authentication agent
/usr/lib/polkit-kde-authentication-agent-1 &

# Keyring-daemon for e.g. ssh keys. Need gnome for this.
gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh

# Workaround for dbus being displayed first
dbus-update-activation-environment GDK_SCALE GDK_DPI_SCALE QT_AUTO_SCREEN_SCALE_FACTOR
