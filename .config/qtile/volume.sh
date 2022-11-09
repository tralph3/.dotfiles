get_volume () {
    pactl list sinks | grep '^[[:space:]]Volume:' | cut -d "/" -f 4 | tr -d ' %'
}

raise_volume () {
    pactl set-sink-volume 0 +$1%
    VOLUME=$(get_volume)
    bar=$(seq -s "─" $(($VOLUME / 5)) | sed 's/[0-9]//g')
    dunstify -I /usr/share/icons/Papirus-Dark/16x16@2x/panel/audio-volume-high.svg -u normal -r 2593 "    $bar"
}
raise_volume () {
    pactl set-sink-volume 0 +$1%
    show_notification
}

lower_volume () {
    pactl set-sink-volume 0 -$1%
    show_notification
}

show_notification () {
    VOLUME=$(get_volume)
    bar=$(seq -s "─" $(($VOLUME / 5)) | sed 's/[0-9]//g')
    dunstify -t 1000 -I /usr/share/icons/Papirus-Dark/16x16@2x/panel/audio-volume-high.svg -u normal -r 2593 "    $bar"
}

case $1 in
    raise) raise_volume $2 ;;
    lower) lower_volume $2 ;;
esac
