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
                paru --sudoloop -R $@ && paru --sudoloop -c --noconfirm
                ;;
            ubuntu)
                sudo apt purge $@ && sudo apt autoremove
                ;;
            fedora)
                dnf remove $@
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


uall (){
    case $DISTRO in
        arch)
            paru --combinedupgrade --sudoloop --skipreview -Syu;
                nvim -c 'autocmd User PackerComplete quitall' -c 'PackerSync';
                paru --sudoloop -c --removemake --noconfirm
            ;;
        ubuntu)
            sudo apt update && sudo apt upgrade;
                nvim -c 'autocmd User PackerComplete quitall' -c 'PackerSync';
                sudo apt autoremove
            ;;
        fedora)
            sudo dnf upgrade;
                nvim -c 'autocmd User PackerComplete quitall' -c 'PackerSync'
            ;;
    esac
}

# run windows executable in own prefix
wn() {
    if [ $1 ] ; then
        WINEPREFIX=$(pwd)/prefix wine $1
    else
        echo "No file provided"
    fi
}
# regular list
alias ls='exa -lg --icons --header --group-directories-first'
# list all
alias la='exa -lag --icons --header --group-directories-first'
# list recursively with depth of 1
alias lr='exa -lTg -L 2 --icons --header --group-directories-first'
# list recursively indefinitely
alias lR='exa -lTg --icons --header --group-directories-first'
# copy terminal stuff to clipboard
alias clip='xclip -selection clipboard'

# file editing
export EDITOR=nvim

alias zshrc="${=EDITOR} ~/.zshrc" # Quick access to the ~/.zshrc file
alias aliasrc="${=EDITOR} ~/.zsh/aliasrc.zsh" # Quick access to this file
alias nvimrc="cd ~/.config/nvim; ${=EDITOR} ~/.config/nvim/init.lua; cd - &> /dev/null" # Quick access to init.lua
alias alarc="${=EDITOR} ~/.config/alacritty/alacritty.yml" # Quick access to alacritty config

gp() {
    git add -A
    git commit -m "$*"
    git push
}

alias localip='ip -brief -color address'

# Recording device
alias record='pactl load-module module-null-sink sink_name="nullsink" sink_properties=device.description="NullSink"'
alias syncmusic='rsync -av /mnt/Storage/Music/ T430:/mnt/Storage/Music'

# get thumbnail from a video
alias get-thumbnail="youtube-dl --write-thumbnail --skip-download"
