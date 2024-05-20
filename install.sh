#!/bin/bash
DOTFILES_DIR=$(dirname "$(realpath "$0")")
"$DOTFILES_DIR/scripts/tangle_all_configs.sh"
