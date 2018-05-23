if [[ -z "$done_env_config" ]]; then

    export PATH="${PATH}:${HOME}/.cargo/bin"
    export PATH="${PATH}:${HOME}/.local/bin"
    export PATH="${PATH}:${HOME}/.sli/bin"
    export PATH="${PATH}:${HOME}/opt/android-sdk/platform-tools"
    export PATH="${PATH}:${HOME}/opt/android-sdk/tools"
    export EDITOR=emacs

    export done_env_config=true
fi
