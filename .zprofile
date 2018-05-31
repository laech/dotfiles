
# Requires ssh-agent for ForwardAgent

if [[ ! -e "${SSH_AUTH_SOCK}" ]]; then
    type ssh-agent \
       && echo "Starting ssh-agent..." \
       && eval "$(ssh-agent -s)" > /dev/null \
       && trap "kill ${SSH_AGENT_PID}" EXIT
fi

# Stop blinking dead links in ls output
# Need this for Fedora as it blinks by default via /etc/DIR_COLORS
eval $(dircolors)
