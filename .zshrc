
# To find out key sequence, type Ctrl-v on terminal then the key
bindkey "^[[1~" beginning-of-line # Home
bindkey "^[[4~" end-of-line       # End
bindkey "^[[3~" delete-char       # Delete
bindkey "^[[2~" overwrite-mode    # Insert

setopt auto_cd
setopt correct_all
setopt prompt_subst

setopt share_history
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_ignore_space
export HISTSIZE=2000
export HISTFILE="${HOME}/.history"
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

    if [[ -f   /etc/bash_completion.d/git-prompt ]]; then
	source /etc/bash_completion.d/git-prompt

    elif [[ -f /usr/local/etc/bash_completion.d/git-prompt.sh ]]; then
	source /usr/local/etc/bash_completion.d/git-prompt.sh

    elif [[ -f /usr/share/git-core/contrib/completion/git-prompt.sh ]]; then
	source /usr/share/git-core/contrib/completion/git-prompt.sh
    fi
fi

precmd() {
    GIT_PS1_DESCRIBE_STYLE="branch"
    GIT_PS1_SHOWUPSTREAM="verbose"
    __git_ps1 "%B%F{28}%n@%m%f %F{25}%1~%f%F{92}" "%f%b $ "
}

if [[ "${OSTYPE}" == darwin* ]]; then
    alias ls='ls -G'
else
    alias ls='ls --color=auto'
fi

alias l='ls -1'
alias ll='ls -lh'
alias la='ll -a'

alias config='git --git-dir="${HOME}/.cfg" --work-tree="${HOME}"'

# This needs to be at the end for it to work.
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
