#!/bin/bash

DOTFILES_DIR=$(dirname "$(realpath "$0")")

NO_CONFIRM=0

if [ -n "$1" ]; then
    case $1 in
        --noconfirm)
            NO_CONFIRM=1
            ;;
    esac
fi

install_config() {
    unset REPLY
    # first argument is relative to dotfiles dir
    ORIGIN=$DOTFILES_DIR/$1
    DEST=$2

    # if there's nothing, install
    if ! [ -a "$DEST" ]; then
        mkdir -pv "$(dirname "$DEST")"
        ln -sfv "$ORIGIN" -T "$DEST"
        # empty line
        echo
    # if they're not the same file, ask
    elif ! [ "$ORIGIN" -ef "$DEST" ]; then
        if [ $NO_CONFIRM -eq 0 ]; then
            echo "$DEST already exists"
            read -p "Overwrite? [y/N]: " -n 1 -r
            # emtpy line
            echo
            overwrite
        else
            overwrite
        fi
    fi
}

overwrite() {
    if [[ "$REPLY" =~ ^[Yy]$ ]] || [ $NO_CONFIRM -eq 1 ]; then
        mkdir -pv "$(dirname "$DEST")"
        rm -rfv "$DEST"
        ln -sfv "$ORIGIN" -T "$DEST"
        # empty line
        echo
    fi
}

# zsh
install_config .zshrc ~/.zshrc
install_config .zsh ~/.zsh
install_config .p10k.zsh ~/.p10k.zsh

# neofetch
install_config .config/neofetch/config.conf ~/.config/neofetch/config.conf

# neovim
install_config .config/nvim ~/.config/nvim

# alacritty
install_config .config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

# htop
install_config .config/htop/htoprc ~/.config/htop/htoprc

# firefox
MOZ_DIR=$(echo ~/.mozilla/firefox/*.default-release)
install_config .mozilla/firefox/chrome "$MOZ_DIR"/chrome
install_config .mozilla/firefox/user.js "$MOZ_DIR"/user.js

# qtile
install_config .config/qtile/archlinux-icon.svg ~/.config/qtile/archlinux-icon.svg
install_config .config/qtile/config.py ~/.config/qtile/config.py
install_config .config/qtile/autostart.sh ~/.config/qtile/autostart.sh

# picom
install_config .config/picom/picom.conf ~/.config/picom/picom.conf

# dunst
install_config .config/dunst/dunstrc ~/.config/dunst/dunstrc

# thunar
install_config .config/xfce4 ~/.config/xfce4
install_config .config/Thunar ~/.config/Thunar

# gtk
install_config .config/gtk-3.0 ~/.config/gtk-3.0

# rofi
install_config .config/rofi ~/.config/rofi

