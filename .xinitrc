xrdb -merge -I"${HOME}" ~/.Xresources

[[ -f /usr/share/X11/xkb/symbols/emacs ]] && setxkbmap -option '' emacs

if [[ -d /etc/X11/xinit/xinitrc.d ]]; then
    for file in /etc/X11/xinit/xinitrc.d/*; do
        [[ -x "$file" ]] && . $file
    done
    unset file
fi

# Using dbus-lauch makes gnome-* apps (terminal etc) work
# and play/pause etc media keys work automatically too
exec dbus-launch --exit-with-session i3
