
if [[ -t 0 && $(tty) == /dev/tty1 && ! $DISPLAY ]]; then
    exec startx
fi
