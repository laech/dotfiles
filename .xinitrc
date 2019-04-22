xrdb -merge -I"${HOME}" ~/.Xresources

setxkbmap -option '' 'emacs-linacs(pc)'

if [[ -d /etc/X11/xinit/xinitrc.d ]]; then
    for file in /etc/X11/xinit/xinitrc.d/*.sh; do
        [[ -x "$file" ]] && . $file
    done
    unset file
fi

if [[ -d "$HOME/.profiles/default/xinitrc" ]]; then
    for file in "$HOME/.profiles/default/xinitrc/"*.sh; do
        [[ -x "$file" ]] && . $file
    done
    unset file
fi

exec i3
