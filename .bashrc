# If not running interactively, don't do anything more
[ -z "$PS1" ] && return

DEBEMAIL=julien
DEBEMAIL=$DEBEMAIL@
DEBEMAIL=${DEBEMAIL}palard.fr
DEBFULLNAME="Julien Palard"
PATH="$HOME/.local/bin:$PATH"

shopt -s cdspell
shopt -s dirspell
shopt -s autocd
shopt -s globstar
shopt -s nocaseglob

if [ -n "$DISPLAY" ]
then
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

export HISTCONTROL=ignoredups
export HISTFILESIZE=5000
export HISTSIZE=5000

umask 022
eval "`dircolors`"

HOSTNAME_SUM=$(cksum <(hostname) | cut -d' ' -f1)
HOSTNAME_BOLD=$(( ($HOSTNAME_SUM + 1) % 2))
HOSTNAME_HUE=$(( ($HOSTNAME_SUM + 3) % 6 + 31))

USERNAME_SUM=$(($(cksum <(whoami) | cut -d' ' -f1) + 5))  # + 5 so root gots red.
USERNAME_BOLD=$(( ($USERNAME_SUM + 1) % 2))
USERNAME_HUE=$(( ($USERNAME_SUM + 2) % 6 + 31))

HOSTNAME_COLOR=$'\E'"[$HOSTNAME_BOLD;${HOSTNAME_HUE}m"
USERNAME_COLOR=$'\E'"[$USERNAME_BOLD;${USERNAME_HUE}m"

WHITE=$'\E[00m'

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"


if ! [ -f "$HOME/.git-prompt.sh" ]
then
    wget -q -O "$HOME/.git-prompt.sh" https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
fi

alias ls='ls --color=auto'
alias fingerprint='find /etc/ssh -name "*.pub" -exec ssh-keygen -l -f {} \;'
alias rekey='ssh-add -e /usr/lib/x86_64-linux-gnu/opensc-pkcs11.so; ssh-add -s /usr/lib/x86_64-linux-gnu/opensc-pkcs11.so'

export PYTHONDEVMODE=y

for extra in /etc/bash_completion ~/.bash_aliases ~/.my_bashrc ~/.git-prompt.sh
do
    if [ -f "$extra" ]
    then
        . $extra
    fi
done

title()
{
    local TITLE="\[\e]0;$1\a\]"
    local PREV_FAIL="\`PREV_FAIL=\$?; if [ \$PREV_FAIL != 0 ]; then echo \[\e[31m\]\$PREV_FAIL \[\e[0m\]; fi\`"
    PS1="$TITLE$PREV_FAIL\[$USERNAME_COLOR\]\u\[$WHITE\]@\[$HOSTNAME_COLOR\]\H\[$WHITE\]:\[\e[32m\]\w\[$WHITE\]"'$(__git_ps1 " (%s)")\n\$ '
}

[ "$TERM" != 'linux' -a z"$TERM" != z'eterm-color' ] && DEFAULT_TITLE="\H \W" || DEFAULT_TITLE=''
title "$DEFAULT_TITLE"

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

unalias venv 2>/dev/null
venv()
{
    deactivate 2>/dev/null
    if ! [[ -d .venv ]]
    then
        python3 -m venv --prompt "$(basename "$PWD")" .venv
    fi
    source .venv/bin/activate
    pip install --upgrade --pre black jedi wheel pip
}

export PIP_REQUIRE_VIRTUALENV=1

dotfiles()
{
    local CLONE="$HOME/.config/dotfiles-repo/"
    mkdir -p ~/.config/
    if [ -d "$CLONE" ]
    then
        git -C "$CLONE" pull --ff-only
    else
        git clone -q https://github.com/JulienPalard/dotfiles.git "$CLONE"
    fi
    "$CLONE"/interactive_copy.py "$CLONE" ~/ --exclude README.md .git interactive_copy.py install.sh __pycache__
    rm -f "$HOME/.git-prompt.sh"
    wget -q -O "$HOME/.git-prompt.sh" https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
}

pasee()
{
    local LOGIN
    local PASSWORD
    local HOST="${1:-https://id.meltygroup.com/tokens/?idp=meltygroup}"
    local TEMP_DIR="$(mktemp --directory --suffix=pasee)"
    read -p 'Login: ' LOGIN
    read -s -p "Password for $LOGIN: " PASSWORD
    echo
    curl -w '%{stderr}%{http_code}' -s -XPOST -d '{"login": "'"$LOGIN"'", "password": "'"$PASSWORD"'"}' "$HOST" > "$TEMP_DIR/stdout" 2> "$TEMP_DIR/stderr"
    local HTTP_RESPONSE="$(<$TEMP_DIR/stdout)"
    local STATUS_CODE="$(<$TEMP_DIR/stderr)"
    JWT="$(jq -r ".access_token" <<< "$HTTP_RESPONSE")"
    if [[ -z "$JWT" || "$STATUS_CODE" != "201" || "$JWT" == "null" ]]; then
        printf "HTTP Error %s: %s\n" "$STATUS_CODE" "$HTTP_RESPONSE"
    fi
    AUTH="Authorization: Bearer $JWT"
    rm -fr "$TEMP_DIR"
}

wyz()
{
    curl https://wyz.fr/ -F"${1##*.}=@$1"
}

compile_python()
{
    # Inspired from the great https://gitlab.com/python-devs/ci-images/
    # Thanks Barry Warsaw.
    local PY_VERSION="$1"
    local BETA="$2"
    local FLAGS=""
    if dpkg --compare-versions "$PY_VERSION" ge 3.8.0  # Since 3.8.0 debug builds are ABI compatible, let's use them.
    then
        FLAGS="--with-pydebug"
    fi
    local URL="https://www.python.org/ftp/python"
    (
        cd /tmp
        wget -qO- $URL/$PY_VERSION/Python-$PY_VERSION$BETA.tgz | tar -xzf - || (
            echo "Version not found, check on $URL."
        )
        [ -d Python-$PY_VERSION$BETA ] && (cd Python-$PY_VERSION$BETA; ./configure $FLAGS --prefix=$HOME/.local/ && make -j 16 && make altinstall) &&
            rm -r Python-$PY_VERSION$BETA
    )
}

compile_all_pythons()
{
    compile_python 3.5.10 &
    compile_python 3.6.12 &
    compile_python 3.7.9 &
    compile_python 3.8.6 &
    compile_python 3.9.1 &
    wait
}

_compile_python()
{
    COMPREPLY=( $( compgen -W '$( command curl -s https://www.python.org/ftp/python/  | grep -o ">[0-9.]\+/<" | sed "s/^>//;s|/<$||" )' -- "${COMP_WORDS[COMP_CWORD]}") )
}


complete -F _compile_python compile_python
