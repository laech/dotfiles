shift-arrow() {
  ((REGION_ACTIVE)) || zle set-mark-command
  zle $1
}
shift-left() shift-arrow backward-char
shift-right() shift-arrow forward-char
shift-up() shift-arrow up-line-or-history
shift-down() shift-arrow down-line-or-history
shift-left-word() shift-arrow backward-word
shift-right-word() shift-arrow forward-word

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

x-delete-char-or-exit() {
  if [[ "$BUFFER" == "" ]]; then
    exit
  fi
  zle delete-char
}

zle -N shift-left
zle -N shift-right
zle -N shift-up
zle -N shift-down
zle -N shift-left-word
zle -N shift-right-word
zle -N x-kill-region
zle -N x-cancel
zle -N x-delete-char-or-exit

bindkey -e '^w' x-kill-region
bindkey -e '^g' x-cancel
bindkey -e '^[' x-cancel
bindkey -e '^u' backward-kill-line
bindkey -e '^h' backward-kill-word

bindkey "^[[3~" x-delete-char-or-exit          # Delete
bindkey "^[[3;5~" kill-word                    # Ctrl-Delete
bindkey "^[[3;6~" kill-line                    # Ctrl-Shift-Delete
bindkey "^[[1;5D" backward-word                # Ctrl-Left
bindkey "^[[1;3D" backward-word                # Alt-Left
bindkey "^[[1;5C" forward-word                 # Ctrl-Right
bindkey "^[[1;3C" forward-word                 # Alt-Right
bindkey "^[[H" beginning-of-line               # Home
bindkey "^[[F" end-of-line                     # End
bindkey "^[[6~" end-of-buffer-or-history       # PageDown
bindkey "^[[5~" beginning-of-buffer-or-history # PageUp
bindkey "^[[Z" reverse-menu-complete           # Shift-Tab
bindkey "^[[1;2A" shift-up
bindkey "^[[1;2B" shift-down
bindkey "^[[1;2D" shift-left
bindkey "^[[1;2C" shift-right
bindkey "^[[1;6D" shift-left-word  # Ctrl-Shift-Left
bindkey "^[[1;6C" shift-right-word # Ctrl-Shift-Right

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

fpath+=~/.zfunc
if [[ ! -e ~/.zfunc/_rustup ]] && hash rustup &>/dev/null; then
  mkdir -p ~/.zfunc
  rustup completions zsh rustup >~/.zfunc/_rustup
  rustup completions zsh cargo >~/.zfunc/_cargo
fi

autoload -U compinit
compinit
zstyle ':completion:*' menu select

# Enable completion from middle of word
# https://stackoverflow.com/questions/22600259/zsh-autocomplete-from-the-middle-of-filename
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

autoload -U bashcompinit
bashcompinit

if ! type __git_ps1 &>/dev/null; then
  . /usr/lib/git-core/git-sh-prompt &>/dev/null
  . /usr/share/git-core/contrib/completion/git-prompt.sh &>/dev/null
fi

readonly my_ps1=$'\n'
readonly my_ps2="%B%F{green}%n@%m%f %F{blue}%~%f%b"
readonly my_ps3=$'\n'"%(?.%%.%F{red}%#%f) "
if type __git_ps1 &>/dev/null; then
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
if type src-hilite-lesspipe.sh &>/dev/null; then
  export LESSOPEN="| src-hilite-lesspipe.sh %s"
else
  export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
fi
export LESS=' -R '

type kubectl >/dev/null && source <(kubectl completion zsh)

alias ls='ls --color=auto'
alias l='ls -1'
alias ll='ls -lh'
alias la='ll -a'

alias tf='terraform'
alias tfi='terraform init'
alias tfp='terraform plan'
alias tfa='terraform apply'
alias tfd='terraform destroy'

alias kb='kubectl'

alias config='git --git-dir="${HOME}/.cfg" --work-tree="${HOME}"'

if [[ ! -z "$WSLENV" ]]; then
  export GPG_TTY=$(tty)
fi
