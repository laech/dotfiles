
if [ -f /etc/bashrc ]; then
      . /etc/bashrc
fi

if [ -f /etc/skel/.bashrc ]; then
      . /etc/skel/.bashrc
fi

# If set, bash does not overwrite an existing file with the >, >&, and <>
# redirection operators. This may be overridden when creating output files by
# using the redirection operator >| instead of >.
set -o noclobber

# If set, a command name that is the name of a directory is executed as if it
# were the argument to the cd command. This option is only used by interactive
# shells.
shopt -s autocd

# If set, minor errors in the spelling of a directory component in a cd
# command will be corrected. The errors checked for are transposed characters,
# a missing character, and one character too many. If a correction is found,
# the corrected file name is printed, and the command proceeds. This option is
# only used by interactive shells.
shopt -s cdspell

# If set, bash checks that a command found in the hash table exists before
# trying to execute it. If a hashed command no longer exists, a normal path
# search is performed.
shopt -s checkhash

# If set, bash checks the window size after each command and, if necessary,
# updates the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, bash attempts to save all lines of a multiple-line command in the
# same history entry. This allows easy re-editing of multi-line commands.
shopt -s cmdhist

# If set, bash attempts spelling correction on directory names during word
# completion if the directory name initially supplied does not exist.
shopt -s dirspell

# If set, bash includes filenames beginning with a '.' in the results of
# pathname expansion.
shopt -s dotglob

#If set, the pattern ** used in a pathname expansion context will match a
# files and zero or more directories and subdirectories. If the pattern is
# followed by a /, only directories and subdirectories match.
shopt -s globstar

# If set, the history list is appended to the file named by the value of the
# HISTFILE variable when the shell exits, rather than overwriting the file.
shopt -s histappend

# If set, bash matches filenames in a case-insensitive fashion when performing
# pathname expansion.
shopt -s nocaseglob


# If set to on, readline performs filename matching and completion in a
# case-insensitive fashion.
bind 'set completion-ignore-case on'

# If set to on, and completion-ignore-case is enabled, readline treats
# hyphens (-) and underscores (_) as equivalent when performing
# case-insensitive filename matching and completion.
bind 'set completion-map-case on'

bind 'set mark-directories on'
bind 'set mark-symlinked-directories on'

# This alters the default behavior of the completion functions in a fashion
# similar to show-all-if-ambiguous. If set to on, words which have more than
# one possible completion without any possible partial completion (the
# possible completions don't share a common prefix) cause the matches to be
# listed immediately instead of ringing the bell.
bind 'set show-all-if-ambiguous on'

# If set to On, menu completion displays the common prefix of the list of
# possible  completions  (which may be empty) before cycling through the list.
bind 'set menu-complete-display-prefix on'

# Similar to complete, but replaces the word to be completed with a single match
# from the list of possible completions. Repeated execution of menu-complete
# steps through the list of possible completions, inserting each match in turn.
# At the end of the list of completions, the bell is rung (subject to the setting
# of bell-style) and the original text is restored.
# An argument of n moves n positions forward in the list of matches; a negative
# argument may be used to move backward through the list. This command is intended
# to be bound to TAB, but is unbound by default.
bind tab:menu-complete
bind control-tab:menu-complete-backward

# Perform history expansion on the current line and insert a space.
# e.g. !!<space>
# !n   # Refer to command line n.
# !-n  # Refer to the current command line minus n.
# !!   # Refer to the previous command. This is a synonym for '!-1'.
# !string      # Refer to the most recent command starting with string.
# !?string[?]  # Refer to the most recent command containing string.
#              # The trailing ? may be omitted if string is followed
#              # immediately by a newline.
# ^ string1 ^ string2 ^  # Quick substitution. Repeat the last command,
#                        # replacing string1 with string2. Equivalent to
#                        # ''!!:s/string1/string2/'' (see Modifiers below).
# !#                     # The entire command line typed so far.
bind space:magic-space

# Other useful key bindings:
#
# tab-insert (C-v TAB)
#     Insert a tab character.
#
# transpose-chars (C-t)
#     Drag the character before point forward over the character at point,
#     moving point forward as well. If point is at the end of the line, then
#     this transposes the two characters before point. Negative arguments have
#     no effect.
# transpose-words (M-t)
#     Drag the word before point past the word after point, moving point over
#     that word as well. If point is at the end of the line, this transposes
#     the last two words on the line.
#
# upcase-word (M-u)
#     Uppercase the current (or following) word. With a negative argument,
#     uppercase the previous word, but do not move point.
# downcase-word (M-l)
#     Lowercase the current (or following) word. With a negative argument,
#     lowercase the previous word, but do not move point.
# capitalize-word (M-c)
#     Capitalize the current (or following) word. With a negative argument,
#     capitalize the previous word, but do not move point.
#
# possible-completions (M-?)
#     List the possible completions of the text before point.
# insert-completions (M-*)
#     Insert all completions of the text before point that would have been
#     generated by possible-completions.
#
# complete-filename (M-/)
#     Attempt filename completion on the text before point.
# possible-filename-completions (C-x /)
#     List the possible completions of the text before point,
#     treating it as a filename.
#
# complete-variable (M-$)
#     Attempt completion on the text before point, treating it as a shell
#     variable.
# possible-variable-completions (C-x $)
#     List the possible completions of the text before point, treating it as
#     a shell variable.


HISTSIZE=1000
HISTCONTROL=ignoredups:erasedups

# https://unix.stackexchange.com/questions/18212/bash-history-ignoredups-and-erasedups-setting-conflict-with-common-history
# https://www.gnu.org/software/bash/manual/html_node/Bash-History-Builtins.html
# history -a # Append the new history lines to the history file.
#            # These are history lines entered since the beginning of the
#            # current Bash session, but not already appended to the history file.
# history -n # Append the history lines not already read from the history file
#            # to the current history list. These are lines appended to the
#            # history file since the beginning of the current Bash session.
# history -w # Write out the current history list to the history file.
# history -c # Clear the history list.
# history -r # Read the history file and append its contents to the history
#            # list.
PROMPT_COMMAND="history -a; history -c; history -r"

if hash __git_ps1 &> /dev/null; then
    #source /etc/bash_completion.d/git-prompt
    GIT_PS1_SHOWUPSTREAM="verbose"
    PROMPT_COMMAND="${PROMPT_COMMAND}; __git_ps1 '\u@\h \W' ' \\\$ '"
fi

PATH="${PATH}:${HOME}/.cargo/bin"
PATH="${PATH}:${HOME}/.local/bin"
PATH="${PATH}:${HOME}/.sli/bin"
PATH="${PATH}:${HOME}/opt/android-sdk/platform-tools"
PATH="${PATH}:${HOME}/opt/android-sdk/tools"

if [[ "${OSTYPE}" == darwin* ]]
then
    alias ls='ls -G'
else
    alias ls='ls --color=auto'
fi

export EDITOR=emacs

alias l='ls -1'
alias ll='ls -lh'
alias la='ll -a'

alias config='git --git-dir="${HOME}/.cfg" --work-tree="${HOME}"'
