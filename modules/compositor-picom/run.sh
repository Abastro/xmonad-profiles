#!/bin/sh
# If the temporary config exists, load it.
if [ -f "$1/picom.conf" ]; then
  echo "Picom configuration found, loading..."
  picom -b --config "$1/picom.conf"
else
  picom -bcCGf --detect-rounded-corners -D 4 -I 0.02 -O 0.02 -e 0.8
fi
