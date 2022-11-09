get_volume () {
    volume=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | cut -d " " -f 2)
    echo $volume*10 | awk -v volume=$volume "{print(volume*100)}"
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
    dunstify -h int:value:$VOLUME -t 1000 -I /usr/share/icons/Papirus-Dark/16x16@2x/panel/audio-volume-high.svg -u normal -r 2593 "Volume"
}

case $1 in
    raise) raise_volume $2 ;;
    lower) lower_volume $2 ;;
esac
