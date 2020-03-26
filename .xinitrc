
if [[ -d /etc/X11/xinit/xinitrc.d ]]; then
    for file in /etc/X11/xinit/xinitrc.d/*.sh; do
        [[ -x "$file" ]] && . $file
    done
    unset file
fi


XFT_DPI=${XFT_DPI:-96}
xrdb -merge <<EOF
rofi.dpi: $XFT_DPI
Xft.dpi: $XFT_DPI
$(cat "$HOME"/.profiles/xresources)
EOF

exec startxfce4
