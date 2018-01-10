[[ -e ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

hash numlockx &> /dev/null && numlockx on

exec i3
