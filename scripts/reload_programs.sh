#!/bin/bash

reload_eww() {
    eww reload
}

reload_qtile() {
    pkill -USR1 qtile
}

reload_dunst() {
    pkill dunst
    dunst & disown
}

reload_emacs () {
    pkill -USR1 emacs
}

reload_waybar() {
    pkill -USR2 waybar
}

reload_all() {
    reload_dunst &
    reload_eww &
    reload_qtile &
    reload_emacs &
    reload_waybar &
}

reload_all
