#!/bin/bash

# Keyboard layout
setxkbmap -layout latam

# Start picom
DISPLAY=":0" /usr/bin/picom --experimental-backends --unredir-if-possible -b

# Set background
/usr/bin/feh --bg-fill /usr/share/backgrounds/linuxmint-una/ddaily_san_francisco.jpg

# Start flameshot
/usr/bin/flameshot &

# Turn numlock on
/usr/bin/numlockx on

# Notifications
dunst &

# Start desktop files in /etc/xdg/autostart and ~/.config/autostart
dex -a
