#!/bin/bash

# Start picom
/usr/bin/picom --experimental-backends -b

# Set background
/usr/bin/feh --bg-fill /usr/share/backgrounds/linuxmint-una/ddaily_san_francisco.jpg

# Start flameshot
/usr/bin/flameshot &

# Turn numlock on
/usr/bin/numlockx on

# Notifications
dunst &
