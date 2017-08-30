# If not running interactively, don't do anything more
[ -z "$PS1" ] && return

DEBEMAIL=julien
DEBEMAIL=$DEBEMAIL@
DEBEMAIL=${DEBEMAIL}palard.fr
DEBFULLNAME="Julien Palard"

shopt -s checkwinsize
shopt -s cdspell
shopt -s dirspell 2>/dev/null # Only in bash 4
shopt -s autocd   2>/dev/null # Only in bash 4
shopt -s globstar 2>/dev/null # Only in bash 4
shopt -s nocaseglob

if [ -n "$DISPLAY" ]; then
    xset b off
fi

# http://nion.modprobe.de/blog/archives/572-less-colors-for-man-pages.html
export LESS_TERMCAP_mb=$'\E[01;31m'    # debut de blink
export LESS_TERMCAP_md=$'\E[01;31m'    # debut de gras
export LESS_TERMCAP_me=$'\E[0m'        # fin
export LESS_TERMCAP_so=$'\E[01;44;33m' # début de la ligne d'état
export LESS_TERMCAP_se=$'\E[0m'        # fin
export LESS_TERMCAP_us=$'\E[01;32m'    # début de souligné
export LESS_TERMCAP_ue=$'\E[0m'        # fin
export DEBEMAIL DEBFULLNAME
export EDITOR=emacs
export LC_ALL='en_US.utf8'

export HISTCONTROL=ignoredups
export HISTFILESIZE=5000
export HISTSIZE=5000

umask 022
eval "`dircolors`"
set -C

# I don't like the default blue (That is too dark for me)
tput initc 12 400 400 1000

HOSTNAME_SUM=$(cksum <(hostname) | cut -d' ' -f1)
HOSTNAME_BOLD=$(( ($HOSTNAME_SUM + 1) % 2))
HOSTNAME_HUE=$(( ($HOSTNAME_SUM + 3) % 6 + 31))

USERNAME_SUM=$(($(cksum <(whoami) | cut -d' ' -f1) + 5))  # + 5 so root gots red.
USERNAME_BOLD=$(( ($USERNAME_SUM + 1) % 2))
USERNAME_HUE=$(( ($USERNAME_SUM + 2) % 6 + 31))

HOSTNAME_COLOR=$'\E'"[$HOSTNAME_BOLD;${HOSTNAME_HUE}m"
USERNAME_COLOR=$'\E'"[$USERNAME_BOLD;${USERNAME_HUE}m"

WHITE=$'\E[00m'

if [ $(id -u) -eq 0 ]
then
    alias rm='rm -i'
    alias cp='cp -i'
    alias mv='mv -i'
fi

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

[ "$TERM" != 'linux' -a z"$TERM" != z'eterm-color' ] && TITLE="\[\033]0;\u@\H:\w\a\]" || TITLE=''
export PS1="$TITLE\[$USERNAME_COLOR\]\u\[$WHITE\]@\[$HOSTNAME_COLOR\]\H\[$WHITE\]"
# .git-prompt.sh is here: https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
if [ -f ~/.git-prompt.sh ]
then
    export PS1="$PS1"'$(__git_ps1 " (%s)")\$ '
else
    export PS1="$PS1"'\$ '
fi

alias ls='ls --color=auto'
alias scr='screen -D -R -U -h 424242'
alias fingerprint='find /etc/ssh -name "*.pub" -exec ssh-keygen -l -f {} \;'
alias rekey='ssh-add -e /usr/lib/x86_64-linux-gnu/opensc-pkcs11.so; ssh-add -s /usr/lib/x86_64-linux-gnu/opensc-pkcs11.so'

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f ~/.my_bashrc ]; then
    . ~/.my_bashrc
fi

if [ -f ~/.git-prompt.sh ]
then
    . ~/.git-prompt.sh
fi

jsonpp()
{
    input="$([ $# -gt 0 ] && printf "%s\n" "$*" || cat -)"
    if ! [ z"$(which pygmentize)" = z"" ]
    then
        printf "%s" "$input" | python3 -mjson.tool | pygmentize -l js || printf "%s\n" "$input"
    else
        printf "%s" "$input" | python3 -mjson.tool || printf "%s\n" "$input"
    fi
}

urldecode()
{
    input="$([ $# -gt 0 ] && printf "%s\n" "$*" || cat -)"
    python3 -c "import urllib.parse, sys; print(urllib.parse.unquote(sys.argv[1]))" "$input"
}

urlencode()
{
    input="$([ $# -gt 0 ] && printf "%s\n" "$*" || cat -)"
    python3 -c "import urllib.parse, sys; print(urllib.parse.quote(sys.argv[1]))" "$input"
}

# Removes *~ and #*# files in curent folder, for a depth limited to 3 folders.
clean()
{
    find -maxdepth 3 -name .emacs_backups -prune \
        -o \( -type f -a \
        \( -name '*~' -o -name '#*#' \) \
        \) \
        -print0 | xargs -0 rm -f
}

workon()
{
    local VENVS="$HOME/.venvs"
    if [ -z "$1" -o z"$1" = z"." ]
    then
        local VENV_NAME="$(basename "$PWD")"
    else
        local VENV_NAME="$1"
    fi
    [ -d "$VENVS/$VENV_NAME" ] || mkdir -p "$VENVS/$VENV_NAME"
    [ -f "$VENVS/$VENV_NAME/bin/activate" ] || python3 -m venv "$VENVS/$VENV_NAME"
    . "$VENVS/$VENV_NAME/bin/activate"
}

_workon()
{
    COMPREPLY=( $( compgen -W '$( command ls "$HOME/.venvs" )' -- "${COMP_WORDS[COMP_CWORD]}") )
}

complete -F _workon workon

# Restore environment variable of existing ssh-agents
ssh-agent-restore()
{
    local QUIET="$1"
    local AGENTS="$(ls -1tr /tmp/ssh-*/* 2>/dev/null)"
    local AUTH_SOCKS_AND_NAME=( )

    if [ z"$?" != z"0" -a -z "$QUIET" ]
    then
        printf "No ssh-agent found.\n" 1>&2
        return 1
    fi
    if [ -z "$QUIET" ]
    then
        for auth_sock in $AGENTS
        do
            AUTH_SOCKS_AND_NAME=( "${AUTH_SOCKS_AND_NAME[@]}" "$(printf "%s " "$auth_sock"; SSH_AUTH_SOCK="$auth_sock" ssh-add -l | cut -d' ' -f3- | tr '\n', ' ')" )
        done
        select AUTH_SOCKS in "${AUTH_SOCKS_AND_NAME[@]}"
        do
            export SSH_AUTH_SOCK="$(printf "%s" "$AUTH_SOCKS" | awk '{print $1}')"
            break
        done
    else
        export SSH_AUTH_SOCK="$(printf "%s" "$AGENTS" | tail -n 1)"
    fi
    export SSH_AGENT_PID="${SSH_AUTH_SOCK##/*/*.}"
}
