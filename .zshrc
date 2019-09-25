
# http://zshwiki.org/home/zle/bindkeys
# Create a zkbd compatible hash.
# To add other keys to this hash, see: man 5 terminfo
typeset -g -A key

[[ -n "$terminfo[khome]" ]] && bindkey "$terminfo[khome]" beginning-of-line
[[ -n "$terminfo[kend]"  ]] && bindkey "$terminfo[kend]"  end-of-line
[[ -n "$terminfo[kich1]" ]] && bindkey "$terminfo[kich1]" overwrite-mode # insert
[[ -n "$terminfo[kdch1]" ]] && bindkey "$terminfo[kdch1]" delete-char

shift-arrow () { ((REGION_ACTIVE)) || zle set-mark-command; zle $1 }
shift-left  () shift-arrow backward-char
shift-right () shift-arrow forward-char
shift-up    () shift-arrow up-line-or-history
shift-down  () shift-arrow down-line-or-history
zle -N shift-left
zle -N shift-right
zle -N shift-up
zle -N shift-down

[[ -n "$terminfo[kLFT]" ]] && bindkey "$terminfo[kLFT]" shift-left
[[ -n "$terminfo[kRIT]" ]] && bindkey "$terminfo[kRIT]" shift-right
[[ -n "$terminfo[kri]"  ]] && bindkey "$terminfo[kri]"  shift-up
[[ -n "$terminfo[kind]" ]] && bindkey "$terminfo[kind]" shift-down

bindkey "^[[1;5D" backward-word  # C-Left
bindkey "^[[1;3D" backward-word  # M-Left
bindkey "^[[1;5C" forward-word # C-Right
bindkey "^[[1;3C" forward-word # M-Right
bindkey "^X^_" redo # C-x C--

x-kill-region() {
    if [[ $REGION_ACTIVE == 0 ]]; then
        zle kill-whole-line
    else
        zle kill-region
    fi
}

x-cancel() {
    if [[ $REGION_ACTIVE == 0 ]]; then
        zle send-break
    else
        zle deactivate-region
    fi
}

zle -N x-kill-region
zle -N x-cancel

bindkey -e '^w' x-kill-region # C-w
bindkey -e '^g' x-cancel      # C-g

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    zle-line-init   () { echoti smkx }
    zle-line-finish () { echoti rmkx }
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
export HISTSIZE=100000
export HISTFILE="${HOME}/.zsh_history"
export SAVEHIST="${HISTSIZE}"

autoload -U select-word-style
select-word-style bash

# Stop blinking dead links in ls output
# Need this for some distros such as Fedora which blinks by default via /etc/DIR_COLORS
eval $(dircolors)

fpath+=~/.zfunc
if [[ ! -e ~/.zfunc/_rustup ]] && hash rustup &> /dev/null; then
    mkdir -p ~/.zfunc
    rustup completions zsh rustup > ~/.zfunc/_rustup
    rustup completions zsh cargo > ~/.zfunc/_cargo
fi

autoload -U compinit
compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==01}:${(s.:.)LS_COLORS}")'

autoload -U bashcompinit
bashcompinit

if ! type __git_ps1 &> /dev/null; then
    . /usr/share/git/completion/git-prompt.sh &> /dev/null \
        || . /usr/lib/git-core/git-sh-prompt &> /dev/null
fi

readonly my_ps1=$'\n'
readonly my_ps2="%B%F{green}%n@%m%f %F{blue}%~%f"
readonly my_ps3=$'\n'"%(?.%%.%F{red}%#%f)%b "
if type __git_ps1 &> /dev/null; then
    precmd() {
        GIT_PS1_DESCRIBE_STYLE="branch"
        GIT_PS1_SHOWUPSTREAM="verbose"
        __git_ps1 "$my_ps1$my_ps2%F{magenta}" "%f$my_ps3"
        # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
    }
else
    PS1="$my_ps1$my_ps2$my_ps3"
fi

alias ls='ls --color=auto'
alias l='ls -1'
alias ll='ls -lh'
alias la='ll -a'

alias config='git --git-dir="${HOME}/.cfg" --work-tree="${HOME}"'

