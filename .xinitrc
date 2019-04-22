
if [[ -d /etc/X11/xinit/xinitrc.d ]]; then
    for file in /etc/X11/xinit/xinitrc.d/*.sh; do
        [[ -x "$file" ]] && . $file
    done
    unset file
fi

exec i3
