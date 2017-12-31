. ~/.bash.conf

hash numlockx &> /dev/null && numlockx on

if [[ -z "${DISPLAY}" && -n "${XDG_VTNR}" && "${XDG_VTNR}" -eq 1 ]]
then
  exec startx
fi
