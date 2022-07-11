#!/bin/bash

# Keyboard layout
setxkbmap -layout latam

# Start picom
DISPLAY=":0" /usr/bin/picom --unredir-if-possible &

# Set background
SCRIPT_DIR=$(dirname "$(realpath "$0")")
$SCRIPT_DIR/background.sh &

# Start flameshot
/usr/bin/flameshot &

# Turn numlock on
/usr/bin/numlockx on

# Notifications
dunst &

# Start desktop files in /etc/xdg/autostart and ~/.config/autostart
dex -a &
