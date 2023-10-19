#!/bin/bash

COLORSCHEME_DIRECTORY="$DOTFILES_DIR/assets/colorschemes"

show_colorscheme_chooser() {
    COLORSCHEME=$(\
        basename -s .el $(ls -1 $COLORSCHEME_DIRECTORY) |\
        rofi -dmenu -p "Choose colorscheme"
    )
    if [[ -z $COLORSCHEME ]]; then
        exit
    fi
}


if [[ -z "$1" ]]; then
    show_colorscheme_chooser
else
    COLORSCHEME="$1"
fi

apply_colorscheme() {
    ln -sf "$COLORSCHEME_DIRECTORY/$COLORSCHEME.el" ~/.config/current_colorscheme
}

generate_config_files() {
    "$DOTFILES_DIR/scripts/tangle_all_configs.sh"
}

reload_all_programs() {
    "$DOTFILES_DIR/scripts/reload_programs.sh"
}

echo "Applying colorscheme \"$COLORSCHEME\""
apply_colorscheme
generate_config_files
reload_all_programs
