#!/bin/bash

DOTFILES_DIR=$(dirname "$(realpath "$0")")
USER_ID=$(id -u)
VARIABLES=$(cat << EOF
#!/bin/bash

export DOTFILES_DIR=$DOTFILES_DIR
export QT_QPA_PLATFORMTHEME=qt5ct
export WALLPAPERS_DIR="/usr/share/my_wallpapers"
export GTK_THEME=Breeze-Dark
EOF
)
DEFAULT_COLORSCHEME=$DOTFILES_DIR/.config/colorschemes/Catppuccin.scheme

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
install_config .config/qtile ~/.config/qtile

# awesome
install_config .config/awesome ~/.config/awesome

# picom
install_config .config/picom/picom.conf ~/.config/picom/picom.conf

# dunst
install_config .config/dunst/dunstrc ~/.config/dunst/dunstrc

# thunar
install_config .config/xfce4 ~/.config/xfce4
install_config .config/Thunar ~/.config/Thunar

# gtk
install_config .config/gtk-3.0 ~/.config/gtk-3.0

# qt
install_config .config/qt5ct ~/.config/qt5ct

# rofi
install_config .config/rofi ~/.config/rofi

# zathura
install_config .config/zathura ~/.config/zathura

# emacs
install_config .config/emacs ~/.config/emacs

# eww
install_config .config/eww ~/.config/eww

# hyprland
install_config .config/hypr ~/.config/hypr

# wpaperd
install_config .config/wpaperd ~/.config/wpaperd
# waybar
install_config .config/waybar ~/.config/waybar

# colorschemes
install_config .config/colorschemes ~/.config/colorschemes

if ! [ -f "~/.config/colorschemes/current_colorscheme/colors.sh" ]; then
    $DOTFILES_DIR/scripts/colorscheme/generate_colorscheme_files.sh $DEFAULT_COLORSCHEME
fi

# dunst colors
install_config .config/colorschemes/current_colorscheme/colors.ini ~/.config/dunst/dunstrc.d/10-colors.conf

if [[ $USER_ID -ne 0 ]]; then
    echo "Root access is needed to create a file that will set environment variables on startup."
    echo "$VARIABLES" | sudo tee /etc/profile.d/dotfiles.sh
else
    echo "$VARIABLES" > /etc/profile.d/dotfiles.sh
fi
