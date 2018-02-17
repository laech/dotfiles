
# Requires ssh-agent for ForwardAgent, see ~/.ssh/config

if [[ -t 0 && $(tty) == /dev/tty1 && ! $DISPLAY ]]; then
    hash ssh-agent &> /dev/null && exec ssh-agent startx || exec startx

elif [[ ! -e "${SSH_AUTH_SOCK}" ]]; then
    hash ssh-agent &> /dev/null \
	&& echo "ssh-agent not available, starting it for this shell..." \
	&& eval "$(ssh-agent -s)" > /dev/null \
	&& trap "kill ${SSH_AGENT_PID}" EXIT
fi
