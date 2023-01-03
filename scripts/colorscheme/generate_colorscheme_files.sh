#!/bin/bash

COLORSCHEME=$1
TEMPLATES_DIR="$DOTFILES_DIR/scripts/colorscheme/templates"

load_colorscheme() {
    if [[ -z "$COLORSCHEME" ]]; then
        echo "No colorscheme path provided."
        exit
    fi
    if [ -f $COLORSCHEME ]; then
        # -a automatically exports the read variables, needed for envsubst
        set -a
        source $COLORSCHEME
        set +a
    else
        echo "Colorscheme doesn't exist. Exiting..."
        exit
    fi
}


generate_all() {
    load_colorscheme

    mkdir -p ~/.config/colorschemes/current_colorscheme

    for i in $(command ls -1 "$TEMPLATES_DIR"); do
        envsubst < $TEMPLATES_DIR/$i >\
            ~/.config/colorschemes/current_colorscheme/colors.$i
    done
}

generate_all
