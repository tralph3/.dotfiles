#!/bin/zsh
# extracting utility
ex () {
    for i in "${@}"; do
        if [ -f $i ] ; then
            DIR_NAME=$(echo "${i%.*}")
            mkdir $DIR_NAME
            cd $DIR_NAME
            case $i in
                *.tar.bz2)   tar xjf ../$i   ;;
                *.tar.gz)    tar xzf ../$i   ;;
                *.tar.xz)    tar xJf ../$i   ;;
                *.bz2)       bunzip2 ../$i   ;;
                *.rar)       unrar x ../$i     ;;
                *.gz)        gunzip ../$i    ;;
                *.tar)       tar xf ../$i    ;;
                *.tbz2)      tar xjf ../$i   ;;
                *.tgz)       tar xzf ../$i   ;;
                *.zip)       unzip ../$i     ;;
                *.Z)         uncompress ../$i;;
                *.7z)        7z x ../$i      ;;
                *)           echo "'$i' has an unrecognized file type." ;;
            esac
        else
            echo "'$i' is not a valid file"
        fi
    done
}

# rmv - to remove packages and its dependencies
rmv () {
    if ! [ $1 ]; then
        echo "No package provided"
        return
    fi
    if command_exists "paru"; then
        paru --sudoloop -Rns $@ && paru --sudoloop -c --noconfirm
    elif command_exists "apt"; then
        sudo apt autoremove --purge $@
    elif command_exists "dnf"; then
        dnf remove $@ && dnf autoremove
    fi
}

# ins - shorter way to install packages
ins () {
    if ! [ $1 ]; then
        echo "No package provided"
        return
    fi
    if command_exists "paru"; then
        paru --sudoloop --skipreview -S $@
        paru --sudoloop -c --removemake --noconfirm
    elif command_exists "apt"; then
        sudo apt install $@; sudo apt autoremove
    elif command_exists "dnf"; then
        dnf install $@
    fi
}

# src - shorter way to search for packages
src () {
    if ! [ $1 ]; then
        echo "No package provided"
        return
    fi
    if command_exists "paru"; then
        paru --bottomup --skipreview --sudoloop "$*"
    elif command_exists "apt"; then
        apt search "$*"
    elif command_exists "dnf"; then
        dnf search "$*"
    fi
}

command_exists() {
    BINARY=$1
    which $BINARY &> /dev/null
    return $?
}

print_result() {
    RESULT=$1
    COMPONENT=$2
    if [[ $RESULT -eq 0 ]]; then
        echo "\x1b[1;32m Successfully updated $COMPONENT\x1b[0m"
    else
        echo "\x1b[1;31m Error updating $COMPONENT\x1b[0m"
    fi
    echo
}

update_component() {
    BINARY=$1
    COMPONENT=$2
    COMMAND=$3
    if ! command_exists $BINARY; then
        return
    fi
    echo "\x1b[1;33m Updating $COMPONENT\x1b[0m"
    eval "$COMMAND"
    RESULT=$?
    print_result $RESULT $COMPONENT
}

uall (){
    update_component "paru" "system packages" "
        paru --combinedupgrade --sudoloop --skipreview -Syu
        paru --sudoloop -c --removemake --noconfirm
    "

    update_component "apt" "system packages" "
        sudo apt dist-upgrade; sudo apt autoremove
    "

    update_component "dnf" "system packages" "
        sudo dnf distro-sync
    "

    update_component "nvim" "neovim plugins" "
        nvim --headless --embed\
            -c 'if !exists(\":PackerSync\") | qall! | endif'\
            -c 'autocmd User PackerComplete qall!'\
            -c 'PackerSync'
    "

    update_component "rustup" "rust" "
        rustup update
    "
}

wn() {
    if [ $1 ] ; then
        WINEPREFIX=$(pwd)/prefix wine $1
    else
        echo "No file provided"
    fi
}

alias ls='eza -lg --icons --header --group-directories-first'
alias la='eza -lag --icons --header --group-directories-first'
alias lr='eza -lTg -L 2 --icons --header --group-directories-first'
alias lR='eza -lTg --icons --header --group-directories-first'

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
