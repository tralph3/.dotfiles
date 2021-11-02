#!/bin/bash

# zsh
ln -sf ~/.dotfiles/.zshrc ~
ln -sf ~/.dotfiles/.zsh ~
ln -sf ~/.dotfiles/.p10k.zsh ~

# neofetch
mkdir -pv ~/.config/neofetch
ln -sf ~/.dotfiles/.config/neofetch/config.conf ~/.config/neofetch/config.conf

# neovim
mkdir -pv ~/.config/nvim
ln -sf ~/.dotfiles/.config/nvim/init.vim ~/.config/nvim/init.vim

# konsole
mkdir -pv ~/.local/share/konsole
ln -sf ~/.dotfiles/.local/share/konsole ~/.local/share
ln -sf ~/.dotfiles/.config/konsolerc ~/.config
