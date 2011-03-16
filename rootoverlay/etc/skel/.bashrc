
# Check for an interactive session
[ -z "$PS1" ] && return

alias ls='ls --color=auto'
export EDITOR="vim"
PS1='[\e[0;31m\u\e[m@\h \W]\$ '
