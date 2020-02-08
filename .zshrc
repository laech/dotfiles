
shift-arrow () { ((REGION_ACTIVE)) || zle set-mark-command; zle $1 }
shift-left  () shift-arrow backward-char
shift-right () shift-arrow forward-char
shift-up    () shift-arrow up-line-or-history
shift-down  () shift-arrow down-line-or-history
zle -N shift-left
zle -N shift-right
zle -N shift-up
zle -N shift-down

# http://zshwiki.org/home/zle/bindkeys
# Create a zkbd compatible hash.
# To add other keys to this hash, see: man 5 terminfo
# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -g -A key

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[ShiftTab]="${terminfo[kcbt]}"
key[ShiftLeft]="${terminfo[kLFT]}"
key[ShiftRight]="${terminfo[kRIT]}"
key[ShiftUp]="${terminfo[kri]}"
key[ShiftDown]="${terminfo[kind]}"

[[ -n "${key[Home]}"       ]] && bindkey -- "${key[Home]}"       beginning-of-line
[[ -n "${key[End]}"        ]] && bindkey -- "${key[End]}"        end-of-line
[[ -n "${key[Insert]}"     ]] && bindkey -- "${key[Insert]}"     overwrite-mode
[[ -n "${key[Backspace]}"  ]] && bindkey -- "${key[Backspace]}"  backward-delete-char
[[ -n "${key[Delete]}"     ]] && bindkey -- "${key[Delete]}"     delete-char
[[ -n "${key[Up]}"         ]] && bindkey -- "${key[Up]}"         up-line-or-history
[[ -n "${key[Down]}"       ]] && bindkey -- "${key[Down]}"       down-line-or-history
[[ -n "${key[Left]}"       ]] && bindkey -- "${key[Left]}"       backward-char
[[ -n "${key[Right]}"      ]] && bindkey -- "${key[Right]}"      forward-char
[[ -n "${key[PageUp]}"     ]] && bindkey -- "${key[PageUp]}"     beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"   ]] && bindkey -- "${key[PageDown]}"   end-of-buffer-or-history
[[ -n "${key[ShiftTab]}"   ]] && bindkey -- "${key[ShiftTab]}"   reverse-menu-complete
[[ -n "${key[ShiftUp]}"    ]] && bindkey -- "${key[ShiftUp]}"    shift-up
[[ -n "${key[ShiftDown]}"  ]] && bindkey -- "${key[ShiftDown]}"  shift-down
[[ -n "${key[ShiftLeft]}"  ]] && bindkey -- "${key[ShiftLeft]}"  shift-left
[[ -n "${key[ShiftRight]}" ]] && bindkey -- "${key[ShiftRight]}" shift-right

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
	autoload -Uz add-zle-hook-widget
	function zle_application_mode_start { echoti smkx }
	function zle_application_mode_stop { echoti rmkx }
	add-zle-hook-widget -Uz zle-line-init zle_application_mode_start
	add-zle-hook-widget -Uz zle-line-finish zle_application_mode_stop
fi

bindkey "^[[3;5~" delete-word # C-Delete
bindkey "^[[1;5D" backward-word  # C-Left
bindkey "^[[1;3D" backward-word  # M-Left
bindkey "^[[1;5C" forward-word # C-Right
bindkey "^[[1;3C" forward-word # M-Right
bindkey "^X^_" redo # C-x C--

update-clipboard() {
    [[ -n "$DISPLAY" ]] \
        && type xsel &> /dev/null \
        && print -rn $CUTBUFFER | xsel -ib
}

update-killring() {
    local content=

    if [[ -z "$DISPLAY" ]] || ! type xsel &> /dev/null; then
        return
    fi

    content=$(xsel -ob)
    if [[ -z "$content" ]] || [[ "$CUTBUFFER" == "$content" ]]; then
        return
    fi

    killring=("$CUTBUFFER" "${killring[@]}")
    CUTBUFFER="$content"
}

x-backward-kill-word() {
    zle backward-kill-word
    update-clipboard
}

x-kill-word() {
    zle kill-word
    update-clipboard
}

x-kill-line() {
    zle kill-line
    update-clipboard
}

x-kill-whole-line() {
    zle kill-whole-line
    update-clipboard
}

x-kill-region() {
    if [[ $REGION_ACTIVE == 0 ]]; then
        zle kill-whole-line
    else
        zle kill-region
    fi
    update-clipboard
}

x-copy-region-as-kill() {
    zle copy-region-as-kill
    update-clipboard
}

x-cancel() {
    if [[ $REGION_ACTIVE == 0 ]]; then
        zle send-break
    else
        zle deactivate-region
    fi
}

x-yank() {
    update-killring
    zle yank
    zle -f yank # Make subsequent yank-pop work
}

zle -N x-kill-word
zle -N x-backward-kill-word
zle -N x-kill-line
zle -N x-kill-whole-line
zle -N x-kill-region
zle -N x-copy-region-as-kill
zle -N x-cancel
zle -N x-yank

bindkey -e "^[d" x-kill-word
bindkey -e "^[^?" x-backward-kill-word
bindkey -e "^[^H" x-backward-kill-word
bindkey -e '^k' x-kill-line
bindkey -e '^u' x-kill-whole-line
bindkey -e '^w' x-kill-region
bindkey -e '^[w' x-copy-region-as-kill
bindkey -e '^g' x-cancel
bindkey -e '^y' x-yank

bindkey '^[[3;2~' x-kill-region # Shift Delete
bindkey '^[[2;5~' x-copy-region-as-kill # Control Insert
bindkey '^[[2;2~' x-yank # Shift Insert

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

# Enable completion from middle of word
# https://stackoverflow.com/questions/22600259/zsh-autocomplete-from-the-middle-of-filename
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==01}:${(s.:.)LS_COLORS}")'

autoload -U bashcompinit
bashcompinit

if ! type __git_ps1 &> /dev/null; then
    . /usr/share/git/completion/git-prompt.sh &> /dev/null \
        || . /usr/lib/git-core/git-sh-prompt &> /dev/null
fi

readonly my_ps1=$'\n'
readonly my_ps2="%B%F{green}%n@%m%f %F{blue}%~%f%b"
readonly my_ps3=$'\n'"%(?.%%.%F{red}%#%f) "
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

# Syntax highlighting for 'less'
export LESSOPEN="| src-hilite-lesspipe.sh %s"
export LESS=' -R '

type aws_zsh_completer.sh > /dev/null && source aws_zsh_completer.sh
type kubectl > /dev/null && source <(kubectl completion zsh)
type helm > /dev/null && source <(helm completion zsh)
type pandoc > /dev/null && source <(pandoc --bash-completion)

alias ls='ls --color=auto'
alias l='ls -1'
alias ll='ls -lh'
alias la='ll -a'
alias tf='terraform'
alias kb='kubectl'

alias config='git --git-dir="${HOME}/.cfg" --work-tree="${HOME}"'
