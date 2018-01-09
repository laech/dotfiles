
if [ -f ~/.bashrc ]; then
 . ~/.bashrc
fi


if [[ -z "${TMUX}" ]]
then
  setleds -D +num
fi

if [[ -z "${TMUX}" && -z "${DISPLAY}" && -n "${XDG_VTNR}" && "${XDG_VTNR}" -eq 1 ]]
then
  exec startx
fi
