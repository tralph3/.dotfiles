#!/bin/zsh
source /etc/os-release

if [ $ID_LIKE ]; then
    DISTRO=$ID_LIKE
else
    DISTRO=$ID
fi


# extracting utility
ex () {
    for i in "${@}"; do
        if [ -f $i ] ; then
            case $i in
                *.tar.bz2)   tar xjf $i   ;;
                *.tar.gz)    tar xzf $i   ;;
                *.tar.xz)    tar xJf $i   ;;
                *.bz2)       bunzip2 $i   ;;
                *.rar)       unrar x $i     ;;
                *.gz)        gunzip $i    ;;
                *.tar)       tar xf $i    ;;
                *.tbz2)      tar xjf $i   ;;
                *.tgz)       tar xzf $i   ;;
                *.zip)       unzip $i     ;;
                *.Z)         uncompress $i;;
                *.7z)        7z x $i      ;;
                *)           echo "'$i' has an unrecognized file type." ;;
            esac
        else
            echo "'$i' is not a valid file"
        fi
    done
}

# rmv - to remove packages and its dependencies
rmv () {
    if [ $1 ] ; then
        case $DISTRO in
            arch)
                paru --sudoloop -Rns $@ && paru --sudoloop -c --noconfirm
                ;;
            ubuntu)
                sudo apt autoremove --purge $@
                ;;
            fedora)
                dnf remove $@ && dnf autoremove
                ;;
        esac
    else
        echo "No package provided"
    fi
}

# ins - shorter way to install packages
ins () {
    if [ $1 ] ; then
        case $DISTRO in
            arch)
                paru --sudoloop --skipreview -S $@;
                    paru --sudoloop -c --removemake --noconfirm
                ;;
            ubuntu)
                sudo apt install $@; sudo apt autoremove
                ;;
            fedora)
                dnf install $@
                ;;
        esac
    else
        echo "No package provided"
    fi
}

# src - shorter way to search for packages
src () {
    if [ $1 ] ; then
        case $DISTRO in
            arch)
                paru --bottomup --skipreview --sudoloop "$*"
                ;;
            ubuntu)
                apt search "$*"
                ;;
            fedora)
                dnf search "$*"
                ;;
        esac
    else
        echo "No package provided"
    fi
}

update_component() {
    COMPONENT_NAME=$1
    COMPONENT_BINARY=$2
    COMPONENT_COMMAND=$3
    if ! [[ -f $(which $(echo $2 | cut -d " " -f1)) ]]; then
        return
    fi
    echo "\x1b[1;33m"
    echo "-------------------------------"
    echo "Updating $1"
    echo "-------------------------------\x1b[0m"
    eval $3
}

uall (){
    update_component "system packages" "paru"\
        "paru --combinedupgrade --sudoloop --skipreview -Syu;
        paru --sudoloop -c --removemake --noconfirm"

    update_component "system packages" "apt"\
        "sudo apt dist-upgrade; sudo apt autoremove"

    update_component "system packages" "dnf" "sudo dnf distro-sync"

    update_component "neovim plugins" "nvim"\
        "nvim -c 'autocmd User PackerComplete quitall'\
        -c 'PackerSync' --headless --embed"

    update_component "rust" "rustup" "rustup update"

    update_component "pip" "pip" "pip install --upgrade pip"
}

wn() {
    if [ $1 ] ; then
        WINEPREFIX=$(pwd)/prefix wine $1
    else
        echo "No file provided"
    fi
}

alias ls='exa -lg --icons --header --group-directories-first'
alias la='exa -lag --icons --header --group-directories-first'
alias lr='exa -lTg -L 2 --icons --header --group-directories-first'
alias lR='exa -lTg --icons --header --group-directories-first'

SESSION_TYPE=$(loginctl show-session\
    $(loginctl -o json | jq --raw-output '.[] .session') -p Type | cut -d= -f2)
case $SESSION_TYPE in
    wayland)
        alias clip='wl-copy'
        ;;
    x11)
        alias clip='xclip -selection clipboard'
        ;;
esac


export EDITOR=nvim

alias zshrc="${=EDITOR} ~/.zshrc"
alias aliasrc="${=EDITOR} ~/.zsh/aliasrc.zsh"
alias nvimrc="cd ~/.config/nvim; ${=EDITOR} ~/.config/nvim/init.lua; cd - &> /dev/null"
alias alarc="${=EDITOR} ~/.config/alacritty/alacritty.yml"

gp() {
    git add -A
    git commit -m "$*"
    git push
}

alias localip='ip -brief -color address'
alias record='pactl load-module module-null-sink sink_name="nullsink" sink_properties=device.description="NullSink"'
