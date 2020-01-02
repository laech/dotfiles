if [[ -z "$done_env_config" ]]; then

    [[ -e "${HOME}/.profiles/default/env" ]] && \
        . "${HOME}/.profiles/default/env"

    export PATH="${PATH}:${HOME}/.cargo/bin"
    export PATH="${PATH}:${HOME}/.local/bin"
    export PATH="${PATH}:${HOME}/.profiles/default/bin"
    export EDITOR=emacs

    export QT_IM_MODULE=ibus
    export GTK_IM_MODULE=ibus
    export XMODIFIERS=@im=ibus

    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
    [ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion"

    export done_env_config=true
fi
