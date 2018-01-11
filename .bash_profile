
if [ -f ~/.bashrc ]
then
 . ~/.bashrc
fi


if [[ -z "${DISPLAY}" && -n "${XDG_VTNR}" && "${XDG_VTNR}" -eq 1 ]]
then
  hash ssh-agent &> /dev/null && exec ssh-agent startx || exec startx
fi
