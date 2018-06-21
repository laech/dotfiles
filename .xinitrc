xrdb -merge -I"${HOME}" ~/.Xresources

[ -f /usr/share/X11/xkb/symbols/emacs ]] && setxkbmap -option '' emacs

# Using dbus-lauch makes gnome-* apps (terminal etc) work
# and play/pause etc media keys work automatically too
exec dbus-launch --exit-with-session i3
