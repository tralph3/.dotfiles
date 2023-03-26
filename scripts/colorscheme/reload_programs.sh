#!/bin/bash

source ~/.config/colorschemes/current_colorscheme/colors.sh

reload_eww() {
    eww reload
}

reload_qtile() {
    pkill -USR1 qtile
}

reload_dunst() {
    DUNST=$(pidof dunst)
    if [[ -z $DUNST && -z $1 ]]; then
        return
    fi

    kill $DUNST
    dunst & disown
}

reload_emacs () {
    pkill -USR1 emacs
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
    reload_eww
    reload_qtile
    reload_emacs
    # reload_mouse
    # reload_keyboard
}
