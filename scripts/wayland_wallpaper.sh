#!/bin/bash

swww init
while true; do
    WALLPAPER=$(find $WALLPAPERS_DIR -maxdepth 1 -type f | shuf --head-count=1)
    swww img $WALLPAPER --transition-step 30 --transition-fps 60 --transition-type random
    sleep 10m
done
