
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line

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

# macOS
for f in $(ls /usr/local/etc/bash_completion.d     2> /dev/null); do
    source   "/usr/local/etc/bash_completion.d/$f" 2> /dev/null
done

# Debian
for f in $(ls /etc/bash_completion.d     2> /dev/null); do
    source   "/etc/bash_completion.d/$f" 2> /dev/null
done

# Fedora
[[ -f /usr/share/git-core/contrib/completion/git-prompt.sh ]] && \
    . /usr/share/git-core/contrib/completion/git-prompt.sh

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
