#!/bin/bash

# zsh
ln -sfv ~/.dotfiles/.zshrc ~
ln -sfv ~/.dotfiles/.zsh ~
ln -sfv ~/.dotfiles/.p10k.zsh ~

# neofetch
mkdir -pv ~/.config/neofetch
ln -sfv ~/.dotfiles/.config/neofetch/config.conf ~/.config/neofetch/config.conf

# neovim
mkdir -pv ~/.config/nvim
ln -sfv ~/.dotfiles/.config/nvim/init.vim ~/.config/nvim/init.vim
ln -sfv ~/.dotfiles/.config/nvim/lua ~/.config/nvim
ln -sfv ~/.dotfiles/.config/nvim/configs ~/.config/nvim

# alacritty
mkdir -pv ~/.config/alacritty
ln -sfv ~/.dotfiles/.config/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml

# htop
mkdir -pv ~/.config/htop
ln -sfv ~/.dotfiles/.config/htop/htoprc ~/.config/htop

# firefox
rm -rf ~/.mozilla/firefox/*.default-release/chrome
ln -sfv ~/.dotfiles/.mozilla/firefox/chrome ~/.mozilla/firefox/*.default-release
ln -sfv ~/.dotfiles/.mozilla/firefox/user.js ~/.mozilla/firefox/*.default-release

# qtile
mkdir -pv ~/.config/qtile
ln -sfv ~/.dotfiles/.config/qtile/config.py ~/.config/qtile
ln -sfv ~/.dotfiles/.config/qtile/autostart.sh ~/.config/qtile

# picom
mkdir -pv ~/.config/picom
ln -sfv ~/.dotfiles/.config/picom/picom.conf ~/.config/picom

# dunst
mkdir -pv ~/.config/dunst
ln -sfv ~/.dotfiles/.config/dunst/dunstrc ~/.config/dunst

# thunar
ln -sfv ~/.dotfiles/.config/xfce4 ~/.config

# gtk
ln -sfv ~/.dotfiles/.config/gtk-3.0 ~/.config

# rofi
mkdir -pv ~/.config/rofi
ln -sfv ~/.dotfiles/.config/rofi ~/.config
