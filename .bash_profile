
if [ -f ~/.bashrc ]
then
 . ~/.bashrc
fi


# Requires ssh-agent for ForwardAgent, see ~/.ssh/config
if [[ -z "${SSH_AGENT_PID}" ]]
then
  hash ssh-agent &> /dev/null && eval "$(ssh-agent -s)" > /dev/null && trap "kill ${SSH_AGENT_PID}" EXIT
fi


if [[ -z "${DISPLAY}" && -n "${XDG_VTNR}" && "${XDG_VTNR}" -eq 1 ]]
then
  exec startx
fi
