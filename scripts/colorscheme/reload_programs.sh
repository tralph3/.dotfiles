#!/bin/bash

source ~/.config/colorschemes/current_colorscheme/colors.sh

reload_qtile() {
    pkill -USR1 qtile
}

reload_dunst() {
    DUNST=$(pidof dunst)
    if [[ -z $DUNST && -z $1 ]]; then
        return
    fi

    kill $DUNST

    cat ~/.config/dunst/dunstrc\
        ~/.config/colorschemes/current_colorscheme/colors.ini |\
        dunst -config - & disown
}

reload_mouse() {
    MOUSE=$(ratbagctl | head -n 1 | cut -d ":" -f 1)
    ratbagctl $MOUSE profile 0 led 0 set color $(echo $BACKGROUND_2 | tr -d \#)
}

reload_keyboard() {
    rgb_keyboard -l reactive-single -b 5 -s 1 -c $(echo $BACKGROUND_2 | tr -d \#)
    sleep 0.1
    dex $DOTFILES_DIR/autostart/setxkbmap.desktop
}

reload_all() {
    reload_dunst
    reload_qtile
    reload_mouse
    reload_keyboard
}
