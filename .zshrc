# Enable Powerlevel10k instant prompt.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

EDITOR=nvim

# Ignore commands that start with a space
setopt HIST_IGNORE_SPACE
setopt appendhistory
setopt INC_APPEND_HISTORY
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zshhistory

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zmodload zsh/complist
compinit

# Include hidden files.
_comp_options+=(globdots)

# Custom keybinds
bindkey '^ ' autosuggest-accept
# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# Load aliases and shortcuts if existent.
[ -f "$HOME/.zsh/aliasrc.zsh" ] && source "$HOME/.zsh/aliasrc.zsh"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# ctrl-left and ctrl-right
bindkey "\e[1;5D" backward-word
bindkey "\e[1;5C" forward-word
# ctrl-bs and ctrl-del
bindkey "\e[3;5~" kill-word
bindkey "\C-_"    backward-kill-word
# del, home and end
bindkey "\e[3~" delete-char
bindkey "\e[H"  beginning-of-line
bindkey "\e[F"  end-of-line
# alt-bs
bindkey "\e\d"  undo

# Reverse search
bindkey '^R' history-incremental-search-backward

# Load plugins
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme
