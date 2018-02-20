
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

source /etc/bash_completion.d/git-prompt
precmd() {
    GIT_PS1_SHOWUPSTREAM="verbose"
    __git_ps1 "%B%F{28}%n@%m%f %F{25}%1~%f%F{92}" "%f%b $ "
}

PATH="${PATH}:${HOME}/.cargo/bin"
PATH="${PATH}:${HOME}/.local/bin"
PATH="${PATH}:${HOME}/.sli/bin"
PATH="${PATH}:${HOME}/opt/android-sdk/platform-tools"
PATH="${PATH}:${HOME}/opt/android-sdk/tools"

if [[ "${OSTYPE}" == darwin* ]]; then
    alias ls='ls -G'
else
    alias ls='ls --color=auto'
fi

export EDITOR=emacs

alias l='ls -1'
alias ll='ls -lh'
alias la='ll -a'

alias config='git --git-dir="${HOME}/.cfg" --work-tree="${HOME}"'
