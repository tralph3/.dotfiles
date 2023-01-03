#!/bin/bash

COLORSCHEME_DIRECTORY="$HOME/.config/colorschemes/"

show_colorscheme_chooser() {
    COLORSCHEME=$(\
        basename -s .scheme $(ls -1 $COLORSCHEME_DIRECTORY*.scheme) |\
        rofi -dmenu -p "Choose colorscheme"
    )
}


if [[ -z "$1" ]]; then
    show_colorscheme_chooser
else
    COLORSCHEME="$1"
fi

echo "Applying colorscheme \"$COLORSCHEME\""
$DOTFILES_DIR/scripts/colorscheme/generate_colorscheme_files.sh $COLORSCHEME_DIRECTORY$COLORSCHEME.scheme
source $DOTFILES_DIR/scripts/colorscheme/reload_programs.sh
reload_all
