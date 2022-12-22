#!/bin/bash

kill $(pidof dunst)

export COLORSCHEME_DIRECTORY="$HOME/.config/colorschemes/"
export COLORSCHEME=$(cat $COLORSCHEME_DIRECTORY/current_colorscheme)
export COLORSCHEME_PATH=$COLORSCHEME_DIRECTORY$COLORSCHEME.scheme

source $COLORSCHEME_PATH

echo "
[global]
    highlight = \"$FOREGROUND_1\"
    frame_color = \"$HIGHLIGHT_1\"

[urgency_low]
    background = \"$BACKGROUND_2\"
    foreground = \"$FOREGROUND_1\"

[urgency_normal]
    background = \"$BACKGROUND_2\"
    foreground = \"$FOREGROUND_1\"

[urgency_critical]
    background = \"$BACKGROUND_2\"
    foreground = \"$FOREGROUND_1\"
    frame_color = \"$HIGHLIGHT_1\"
" | cat - ~/.config/dunst/dunstrc | dunst -config - & disown
