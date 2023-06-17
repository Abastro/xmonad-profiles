#!/bin/sh
echo "Startup for LightDM..."

# picom does transparency
picom -bcCGf -D 4 -I 0.02 -O 0.02 -e 0.8

# ibus-daemon provides ibus input handling
ibus-daemon -drx &

# locker dedicated for lightdm
light-locker &

# TODO: Make this somehow a separate settings
xset s 3000 dpms 3600 3600 3600

# Authentication agent for polkit
/usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 &

# Keyring-daemon for e.g. ssh keys
gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh

# Workaround for dbus being displayed first
dbus-update-activation-environment GDK_SCALE GDK_DPI_SCALE QT_AUTO_SCREEN_SCALE_FACTOR

