if [[ -z "$done_env_config" ]]; then

    export PATH="${PATH}:${HOME}/.cargo/bin"
    export PATH="${PATH}:${HOME}/.local/bin"
    export EDITOR=emacs

    export done_env_config=true
fi
