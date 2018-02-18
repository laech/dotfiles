
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

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' formats       ' %B%F{92}git(%b%f%F{92})%f%%b'
zstyle ':vcs_info:*' actionformats ' %B%F{92}git(%b|%a%f%F{92})%f%%b'
zstyle ':vcs_info:git*+set-message:*' hooks git-st
precmd() { vcs_info }

function +vi-git-st() {
    local ahead
    local behind
    local remote="$(git for-each-ref --format='%(upstream:short)' $(git symbolic-ref -q HEAD))"

    if [[ -n "${remote}" ]]; then
        behind=$(git rev-list HEAD.."${hook_com[branch]}"@{upstream} --count)
        ahead=$(git rev-list "${hook_com[branch]}"@{upstream}..HEAD --count)
	((${behind})) && hook_com[branch]="${hook_com[branch]} %F{red}-${behind}%f"
	((${ahead})) &&  hook_com[branch]="${hook_com[branch]} %F{28}+${ahead}%f"
    fi
}

PROMPT='%B%F{28}%n@%m%f %F{25}%1~%f%b${vcs_info_msg_0_} $ '

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
