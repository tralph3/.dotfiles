PREVIOUS_WALLPAPER_PATH=""
WALLPAPER=
SET_RANDOM=
LOOP=

while getopts "lsw:" opt; do
    case $opt in
    l) LOOP=1 ;;
    s) SET_RANDOM=1 ;;
    w) WALLPAPER=$OPTARG ;;
    ?) echo "Unknown option $opt"; exit 2;;
    esac
done

start_wall_loop() {
    while true; do
        set_random_wallpaper
        sleep 10m
    done
}

set_wallpaper() {
    rsvg-convert "$WALLPAPER" | swww img --transition-step 30 --transition-fps 60 --transition-type random -
}

set_random_wallpaper() {
        WALLPAPER=$(find "$WALLPAPERS_DIR" -maxdepth 1 -type f -not -path "$PREVIOUS_WALLPAPER_PATH" | shuf --head-count=1)
        set_wallpaper
        PREVIOUS_WALLPAPER_PATH="$WALLPAPER"
}

swww-daemon --format xrgb & disown

if ! [ -z "$SET_RANDOM" ]; then
    set_random_wallpaper
fi

if ! [ -z "$WALLPAPER" ]; then
    set_wallpaper
fi

if ! [ -z $LOOP ]; then
    start_wall_loop
fi
