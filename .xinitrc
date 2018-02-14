xrdb -merge -I"${HOME}" ~/.Xresources

hash numlockx &> /dev/null && numlockx on
hash fcitx &> /dev/null && fcitx

exec i3
