#!/bin/sh
gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh
