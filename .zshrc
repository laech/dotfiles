
# To find out key sequence, type Ctrl-v on terminal then the key
bindkey "^[[1~" beginning-of-line # Home
bindkey "^[[4~" end-of-line       # End
bindkey "^[[3~" delete-char       # Delete
bindkey "^[[2~" overwrite-mode    # Insert

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

autoload -U select-word-style
select-word-style bash

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
