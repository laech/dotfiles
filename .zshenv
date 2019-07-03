if [[ -z "$done_env_config" ]]; then

    export PATH="${PATH}:${HOME}/.cargo/bin"
    export PATH="${PATH}:${HOME}/.local/bin"
    export PATH="${PATH}:${HOME}/.profiles/default/bin"
    export PATH="${PATH}:${HOME}/opt/android-sdk/platform-tools"
    export PATH="${PATH}:${HOME}/opt/android-sdk/tools"
    export EDITOR=emacs

    export QT_IM_MODULE=ibus
    export GTK_IM_MODULE=ibus
    export XMODIFIERS=@im=ibus

    export DOTNET_CLI_TELEMETRY_OPTOUT=1 

    export done_env_config=true
fi
