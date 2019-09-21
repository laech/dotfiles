if [[ -z "$done_env_config" ]]; then

    [[ -e "${HOME}/.profiles/default/env" ]] && \
        . "${HOME}/.profiles/default/env"

    export PATH="${PATH}:${HOME}/.cargo/bin"
    export PATH="${PATH}:${HOME}/.local/bin"
    export PATH="${PATH}:${HOME}/.profiles/default/bin"
    export PATH="${PATH}:${HOME}/opt/android-sdk/platform-tools"
    export PATH="${PATH}:${HOME}/opt/android-sdk/tools"
    export EDITOR=emacs

    export QT_IM_MODULE=ibus
    export GTK_IM_MODULE=ibus
    export XMODIFIERS=@im=ibus

    export done_env_config=true
fi
