get_icon_theme_name () {
    grep "gtk-icon-theme-name" ~/.config/gtk-3.0/settings.ini | cut -d "=" -f 2
}

get_volume () {
    mute_check=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | grep -o MUTED)
    if [[ "MUTED" == $mute_check ]]
    then
        volume=0.00
    else
        volume=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | cut -d " " -f 2)
    fi
    echo $volume*10 | awk -v volume=$volume "{print(volume*100)}"
}

get_volume_icon () {
    ICON_THEME=$(get_icon_theme_name)
    if ((0==$1))
    then
        echo "/usr/share/icons/$ICON_THEME/22x22/panel/audio-volume-muted.svg"
    elif ((1<=$1 && $1<=33))
    then
        echo "/usr/share/icons/$ICON_THEME/22x22/panel/audio-volume-low.svg"
    elif ((34<=$1 && $1<=66))
    then
        echo "/usr/share/icons/$ICON_THEME/22x22/panel/audio-volume-medium.svg"
    else
        echo "/usr/share/icons/$ICON_THEME/22x22/panel/audio-volume-high.svg"
    fi
}

get_sink_name () {
    wpctl inspect @DEFAULT_AUDIO_SINK@ | grep node.description | grep -o '".*"' | sed 's/"//g'
}

raise_volume () {
    pactl set-sink-volume 0 +$1%
    show_notification
}

lower_volume () {
    pactl set-sink-volume 0 -$1%
    show_notification
}

toggle_mute_volume () {
    pactl set-sink-mute 0 toggle
    show_notification
}

show_notification () {
    VOLUME=$(get_volume)
    SINK_NAME=$(get_sink_name)
    ICON=$(get_volume_icon $VOLUME)
    dunstify -h int:value:$VOLUME -t 1000 -I $ICON -u normal -r 2593 "$SINK_NAME"
}

case $1 in
    raise) raise_volume $2 ;;
    lower) lower_volume $2 ;;
    mute)  toggle_mute_volume ;;
esac
