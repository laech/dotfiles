xrdb -merge -I"${HOME}" ~/.Xresources

hash setxkbmap && [[ -f /usr/share/X11/xkb/symbols/emacs ]] && setxkbmap -option '' emacs

hash numlockx && numlockx on
hash fcitx && fcitx
hash feh && feh --bg-fill --randomize ~/wallpapers

# Using dbus-lauch makes gnome-* apps (terminal etc) work
# and play/pause etc media keys work automatically too
exec dbus-launch --exit-with-session i3
