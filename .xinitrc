xrdb -merge -I"${HOME}" ~/.Xresources

[[ -f /usr/share/X11/xkb/symbols/custom ]] && setxkbmap -option '' emacs-linacs

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

create-i3-config

# Using dbus-lauch makes gnome-* apps (terminal etc) work
# and play/pause etc media keys work automatically too
exec dbus-launch --exit-with-session i3
