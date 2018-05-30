xrdb -merge -I"${HOME}" ~/.Xresources

# CapsLock is Control, both shift turns on CapsLock
hash setxkbmap && setxkbmap -option ctrl:nocaps -option shift:both_capslock

hash numlockx && numlockx on
hash fcitx && fcitx
hash feh && feh --bg-fill --randomize ~/wallpapers

# Using dbus-lauch makes gnome-* apps (terminal etc) work
# and play/pause etc media keys work automatically too
exec dbus-launch --exit-with-session i3
