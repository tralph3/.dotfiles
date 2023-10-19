#!/bin/bash

DONT_RELOAD=
COLORSCHEME_PATH=
COLORSCHEME_NAME=
COLORSCHEME_DIRECTORY="$DOTFILES_DIR/assets/colorschemes"

while getopts "rc:" opt; do
    case $opt in
    r) DONT_RELOAD=1 ;;
    c) COLORSCHEME_PATH=$OPTARG;;
    ?) echo "Unknown option $opt"; exit 2;;
    esac
done

show_colorscheme_chooser() {
    COLORSCHEME_NAME=$(\
        basename -s .el $(ls -1 $COLORSCHEME_DIRECTORY) |\
        rofi -dmenu -p "Choose colorscheme"
    )
    if [[ -z "$COLORSCHEME_NAME" ]]; then
        exit
    fi
}

if [[ -z "$COLORSCHEME_PATH" ]]; then
    show_colorscheme_chooser
    COLORSCHEME_PATH="$COLORSCHEME_DIRECTORY/$COLORSCHEME_NAME.el"
fi

apply_colorscheme() {
    ln -sf "$COLORSCHEME_PATH" ~/.config/current_colorscheme
}

generate_config_files() {
    "$DOTFILES_DIR/scripts/tangle_all_configs.sh"
}

reload_all_programs() {
    "$DOTFILES_DIR/scripts/reload_programs.sh"
}

if ! [ -f "$COLORSCHEME_PATH" ]; then
    echo "No such file or directory $COLORSCHEME_PATH"
    exit 1
fi

echo "Applying colorscheme \"$COLORSCHEME_PATH\""
apply_colorscheme
echo "Generating config files..."
generate_config_files
if [ -z $DONT_RELOAD ]; then
    echo "Reloading programs..."
    reload_all_programs
fi
