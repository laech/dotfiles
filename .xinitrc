xrdb -merge -I"${HOME}" ~/.Xresources

hash xmodmap && [[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

hash numlockx && numlockx on
hash fcitx && fcitx
hash feh && feh --bg-fill --randomize ~/wallpapers

exec i3
