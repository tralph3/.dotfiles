#!/bin/bash

export COLORSCHEME_DIRECTORY="$HOME/.config/colorschemes/"

show_colorscheme_chooser() {
    export COLORSCHEME=$(\
        basename -s .scheme $(ls -1 $COLORSCHEME_DIRECTORY*.scheme) |\
        rofi -dmenu
    )
}

load_colorscheme() {
    if [ -f $COLORSCHEME_PATH ]; then
        update_current_colorscheme
    else
        echo "Colorscheme doesn't exist. Exiting..."
        exit
    fi
}

update_current_colorscheme() {
    echo "$COLORSCHEME" > "$HOME/.config/colorschemes/current_colorscheme"
}

reload_qtile() {
    pkill -USR1 qtile
}

reload_dunst() {
    $HOME/.dotfiles/scripts/reload_dunst.sh
}

reload_all_configs() {
    reload_dunst
    reload_qtile
}

if [[ -z "$1" ]]; then
    show_colorscheme_chooser
else
    export COLORSCHEME="$1"
fi

export COLORSCHEME_PATH="$COLORSCHEME_DIRECTORY$COLORSCHEME.scheme"

echo "Applying colorscheme \"$COLORSCHEME\""
load_colorscheme
reload_all_configs
