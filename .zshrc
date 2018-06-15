# http://zshwiki.org/home/zle/bindkeys
# Create a zkbd compatible hash.
# To add other keys to this hash, see: man 5 terminfo
typeset -g -A key

key[Home]="$terminfo[khome]"
key[End]="$terminfo[kend]"
key[Insert]="$terminfo[kich1]"
key[Backspace]="$terminfo[kbs]"
key[Delete]="$terminfo[kdch1]"

[[ -n "$key[Home]"      ]] && bindkey -- "$key[Home]"      beginning-of-line
[[ -n "$key[End]"       ]] && bindkey -- "$key[End]"       end-of-line
[[ -n "$key[Insert]"    ]] && bindkey -- "$key[Insert]"    overwrite-mode
[[ -n "$key[Backspace]" ]] && bindkey -- "$key[Backspace]" backward-delete-char
[[ -n "$key[Delete]"    ]] && bindkey -- "$key[Delete]"    delete-char

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init   () { echoti smkx }
    function zle-line-finish () { echoti rmkx }
    zle -N zle-line-init
    zle -N zle-line-finish
fi

# Turn off XOFF/XON to allow C-s to forward search history
[[ $- == *i* ]] && stty -ixon

setopt auto_cd
setopt correct
setopt prompt_subst

setopt share_history
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_ignore_space
export HISTSIZE=2000
export HISTFILE="${HOME}/.zsh_history"
export SAVEHIST="${HISTSIZE}"

# Stop blinking dead links in ls output
# Need this for Fedora as it blinks by default via /etc/DIR_COLORS
if [[ "${OSTYPE}" == linux* ]]; then
    eval $(dircolors)
fi

autoload -U select-word-style
select-word-style bash


if [[ "${OSTYPE}" == darwin* ]]; then
    fpath=(/usr/local/share/zsh-completions $fpath)
fi
autoload -U compinit
compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==01}:${(s.:.)LS_COLORS}")'

autoload -U bashcompinit
bashcompinit

if ! hash __git_ps1 &> /dev/null; then

    # Arch
    if [[ -f   /usr/share/git/completion/git-prompt.sh ]]; then
	source /usr/share/git/completion/git-prompt.sh

    # Fedora
    elif [[ -f /usr/share/git-core/contrib/completion/git-prompt.sh ]]; then
	source /usr/share/git-core/contrib/completion/git-prompt.sh
    fi
fi

precmd() {
    GIT_PS1_DESCRIBE_STYLE="branch"
    GIT_PS1_SHOWUPSTREAM="verbose"
    __git_ps1 "%B%F{blue}%~%f%F{magenta}" " %f%b%(?.%%.%F{red}%#%f) "
}

alias l='ls -1'
alias ll='ls -lh'
alias la='ll -a'

alias config='git --git-dir="${HOME}/.cfg" --work-tree="${HOME}"'

# This needs to be at the end for it to work.
# Arch
[[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] && \
    . /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]
# Fedora
[[ -f /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] && \
    . /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

if [[ "${OSTYPE}" == darwin* ]]; then
    alias ls='ls -G'
    . /usr/local/etc/bash_completion.d/git-prompt.sh
    . /usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh
    . /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    # zsh-syntax-highlighting needs to be at the end for it to work.
else
    alias ls='ls --color=auto'
fi
