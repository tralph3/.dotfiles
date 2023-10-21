#!/bin/bash

DOTFILES_DIR=$(dirname "$(realpath "$0")")
USER_ID=$(id -u)
VARIABLES=$(cat << EOF
#!/bin/bash

export DOTFILES_DIR=$DOTFILES_DIR
export QT_QPA_PLATFORMTHEME=qt5ct
export WALLPAPERS_DIR="/home/tralph3/.local/share/wallpapers"
export GTK_THEME=adw-gtk3
export ZDOTDIR=/home/tralph3/.config/zsh
export ANDROID_HOME=/home/tralph3/.local/share/android
export ANDROID_SDK_ROOT=$ANDROID_HOME
EOF
)

DEFAULT_COLORSCHEME="$DOTFILES_DIR/assets/colorschemes/gruvbox-material-dark-soft.el"

if ! [ -f "~/.config/current_colorscheme" ]; then
    source "$DOTFILES_DIR/scripts/change_colorscheme.sh" "-c$DEFAULT_COLORSCHEME"
fi

source "$DOTFILES_DIR/scripts/tangle_all_configs.sh"

if [[ $USER_ID -ne 0 ]]; then
    echo "Root access is needed to create a file that will set environment variables on startup."
    echo "$VARIABLES" | sudo tee /etc/profile.d/dotfiles.sh
else
    echo "$VARIABLES" > /etc/profile.d/dotfiles.sh
fi
