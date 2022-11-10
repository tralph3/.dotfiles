get_icon_theme_name () {
    grep "gtk-icon-theme-name" ~/.config/gtk-3.0/settings.ini | cut -d "=" -f 2
}

get_brightness () {
    light
}

get_brightness_icon () {
    ICON_THEME=$(get_icon_theme_name)
    if ((0==$1))
    then
        echo "/usr/share/icons/$ICON_THEME/symbolic/status/display-brightness-off-symbolic.svg"
    elif ((1<=$1 && $1<=33))
    then
        echo "/usr/share/icons/$ICON_THEME/symbolic/status/display-brightness-low-symbolic.svg"
    elif ((34<=$1 && $1<=66))
    then
        echo "/usr/share/icons/$ICON_THEME/symbolic/status/display-brightness-medium-symbolic.svg"
    else
        echo "/usr/share/icons/$ICON_THEME/symbolic/status/display-brightness-high-symbolic.svg"
    fi
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
    ICON=$(get_brightness_icon $BRIGHTNESS)
    dunstify -h int:value:$BRIGHTNESS -t 1000 -I $ICON -u normal -r 2593 "Screen Brightness"
}

case $1 in
    raise) raise_brightness $2 ;;
    lower) lower_brightness $2 ;;
esac
