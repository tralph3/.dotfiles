#!/bin/bash

# zsh
ln -sf ~/.dotfiles/.zshrc ~
ln -sf ~/.dotfiles/.zsh ~
ln -sf ~/.dotfiles/.p10k.zsh ~

# neofetch
mkdir -pv ~/.config/neofetch
ln -sf ~/.dotfiles/.config/neofetch/config.conf ~/.config/neofetch/config.conf
