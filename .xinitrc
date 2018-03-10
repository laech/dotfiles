xrdb -merge -I"${HOME}" ~/.Xresources

numlockx on
fcitx
nm-applet &

# https://wiki.archlinux.org/index.php/GNOME/Keyring#PAM_method
dbus-update-activation-environment --systemd DISPLAY
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

exec i3
