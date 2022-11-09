get_brightness () {
    light
}

raise_brightness () {
    light -A $1
    show_notification
}

lower_brightness () {
    light -U $1
    show_notification
}

show_notification () {
    BRIGHTNESS=$(get_brightness)
    dunstify -h int:value:$BRIGHTNESS -t 1000 -I /usr/share/icons/Papirus-Dark/16x16/actions/brightnesssettings.svg -u normal -r 2593 "Brightness"
}

case $1 in
    raise) raise_brightness $2 ;;
    lower) lower_brightness $2 ;;
esac
