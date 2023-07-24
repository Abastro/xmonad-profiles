#!/bin/sh
# Ubuntu
if [ -d "/usr/lib/policykit-1-gnome" ]; then
  /usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 &
# Arch
elif [ -d "/usr/lib/polkit-gnome" ]; then
  /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
else
  echo "policykit-gnome not found."
fi
