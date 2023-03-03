#!/bin/sh

echo "Installs suckless-tools, light-locker, xss-lock, policykit-1-gnome, gnome-keyring"

# Install necessary components (NOTE: picom cannot be installed this way)
sudo apt install suckless-tools light-locker xss-lock policykit-1-gnome gnome-keyring
