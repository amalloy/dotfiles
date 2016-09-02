# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# instead of having C-s do awful things like stop handling input, let it i-search
stty stop undef

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

export PS1="\n\u@\h \w" # waste vertical space to get clarity and horiz space
# Put the above BEFORE xterm settings so window title still works
if [[ "$TERM" != "dumb" ]]; then
    export PS1="$PS1 \[\033[01;33m\]\$(__git_ps1 %s)\[\033[00;38m\]"
fi

export PS1="$PS1\n\$ "

# If this is an xterm set the window title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

export LESS="-R" # Make less handle ansi colors

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

if [ -f /usr/local//etc/bash_completion ] && ! shopt -oq posix; then
    . /usr/local//etc/bash_completion
fi

if [ -f /usr/local/etc/bash_completion.d/git-prompt.sh ] && ! shopt -oq posix; then
    . /usr/local/etc/bash_completion.d/git-prompt.sh
fi

eval $(gpg-agent --daemon 2>/dev/null)

export PATH=~/bin:~/.local/bin:/usr/local/opt/ccache/libexec:/usr/local/opt/coreutils/libexec/gnubin:$PATH
export EDITOR="emacs -nw"

export VIRTUAL_ENV_DISABLE_PROMPT='1'

export GPG_TTY=$(tty)

export LEIN_FAST_TRAMPOLINE=y
alias e='emacs -nw'

export FACTUAL_SRC_DIR=~/src/clojure
export BACK_HOME=~/src/factual/back
export COORDINATOR_HOME=~/src/clojure/neutronic
export EVENTS_HOME=~/src/factual/tasks

# docker mumbo-jumbo
$(boot2docker shellinit 2>/dev/null)

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
awssh() {
    ssh ubuntu@${1} -i ~/.aws/audience.pem -o StrictHostKeyChecking=no
}
aessh() {
    ssh ubuntu@${1} -i ~/.aws/audience_emr.pem -o StrictHostKeyChecking=no
}
