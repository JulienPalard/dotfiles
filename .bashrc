# If not running interactively, don't do anything more
[ -z "$PS1" ] && return

DEBEMAIL=julien
DEBEMAIL=$DEBEMAIL@
DEBEMAIL=${DEBEMAIL}palard.fr
DEBFULLNAME="Julien Palard"

shopt -s cdspell
shopt -s dirspell
shopt -s autocd
shopt -s globstar
shopt -s nocaseglob

PATH="$PATH:$HOME/.local/bin"
CDPATH="~/clones/"
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

export PIP_REQUIRE_VIRTUALENV=1

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

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"


if ! [ -f "$HOME/.git-prompt.sh" ]
then
    wget -q -O "$HOME/.git-prompt.sh" https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
fi

alias ls='ls --color=auto'
alias fingerprint='find /etc/ssh -name "*.pub" -exec ssh-keygen -l -f {} \;'
alias rekey='ssh-add -e /usr/lib/x86_64-linux-gnu/opensc-pkcs11.so >/dev/null 2>&1; ssh-add -s /usr/lib/x86_64-linux-gnu/opensc-pkcs11.so'

export PYTHONDEVMODE=y

for extra in /etc/bash_completion ~/.bash_aliases ~/.my_bashrc ~/.git-prompt.sh ~/clones/mdk/compile-python/compile-python.sh
do
    if [ -f "$extra" ]
    then
        . $extra
    fi
done


python_ps1()
{
    local pypath="$(which python 2>/dev/null)"
    if [[ -z "$pypath" ]] || [[ "$pypath" == "/usr/bin/python" ]]
    then
        return
    fi
    local relative="$(realpath --relative-to=$(pwd) -s "$pypath")"
    if [[ ${#relative} -lt ${#pypath} ]]
    then
        pypath="$relative"
    fi
    printf "$1" "${pypath%/bin/python}"
}

if [[ "$TERM" != 'dumb' ]]
then
    _TITLE="\[\e]0;\H \W\a\]"
else
    _TITLE=''
fi
_PREV_FAIL="\`PREV_FAIL=\$?; if [ \$PREV_FAIL != 0 ]; then echo \[\e[31m\]\$PREV_FAIL \[\e[0m\]; fi\`"

if ! [[ -f ~/.fonts/dejavu/DejaVuSansMonoNerdFontCompleteMono.ttf ]]
then
    mkdir -p ~/.fonts/dejavu/
    wget -qO ~/.fonts/dejavu/DejaVuSansMonoNerdFontCompleteMono.ttf https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/DejaVuSansMono/Regular/complete/DejaVu%20Sans%20Mono%20Nerd%20Font%20Complete%20Mono.ttf
fi

GIT_RED_FG='\e[38;2;244;77;39m'
GIT_RED_BG='\e[48;2;244;77;39m'
PY_BLUE_FG='\e[38;2;53;112;160m'
PY_BLUE_BG='\e[48;2;53;112;160m'
PY_YELLOW_FG='\e[38;2;255;222;87m'
PY_YELLOW_BG='\e[48;2;255;222;87m'
# 🬫🬛
# 🭮🭬
PY_PS1='$(python_ps1 "${PY_BLUE_FG}🭮${PY_BLUE_BG}${PY_YELLOW_FG}  %s \e[0m${PY_BLUE_FG}🭬\e[0m")'
GIT_PS1='$(__git_ps1 "${GIT_RED_FG}🭮${GIT_RED_BG}\e[97m  %s \e[0m${GIT_RED_FG}🭬\e[0m")'
PS1="${_TITLE}${_PREV_FAIL}${USERNAME_COLOR}\u\e[0m@${HOSTNAME_COLOR}\H\e[0m:\e[32m\w\e[0m${PY_PS1}${GIT_PS1}\n\$ "

eval "$(direnv hook bash)"

jsonpp()
{
    input="$([ $# -gt 0 ] && printf "%s\n" "$*" || cat -)"
    if ! [ z"$(which pygmentize)" = z"" ]
    then
        printf "%s" "$input" | python -mjson.tool | pygmentize -l js || printf "%s\n" "$input"
    else
        printf "%s" "$input" | python -mjson.tool || printf "%s\n" "$input"
    fi
}

urldecode()
{
    input="$([ $# -gt 0 ] && printf "%s\n" "$*" || cat -)"
    python -c "import urllib.parse, sys; print(urllib.parse.unquote(sys.argv[1]))" "$input"
}

urlencode()
{
    input="$([ $# -gt 0 ] && printf "%s\n" "$*" || cat -)"
    python -c "import urllib.parse, sys; print(urllib.parse.quote(sys.argv[1]))" "$input"
}

# Removes *~ and #*# files in curent folder, for a depth limited to 3 folders.
clean()
{
    find -maxdepth 3 \
        \( -name '*~' -o -name '#*#' -o -name .tox -o -name .mypy_cache \) \
        -print0 | xargs -0 rm -vfr
}

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

wyz()
{
    curl https://wyz.fr/ -F"${1##*.}=@$1"
}

e()
{
    emacsclient --no-wait "$@"
}

venv()
{
    # direnv-enabled venv creation.
    if ! [[ -d .venv ]]
    then
        "python$1" -m venv .venv
        echo 'PATH=$(pwd)/.venv/bin/:$PATH' >> .envrc
    fi
}

pip-common()
{
    python -m pip install --upgrade --upgrade-strategy eager mypy black flake8 jedi-language-server pylint build twine grip tox pip
}

github-gpg()
{
    curl https://github.com/$1.gpg | gpg --import-options show-only --import -
}

download-random-pypi-proj()
{
    PKG_URL="$(curl -s https://pypi.org/rss/packages.xml | grep -o 'https://pypi.org/project/[^ <]*' | shuf | head -n 1 | sed 's/project/simple/g')"
    ARCHIVE_URL="$(curl -s "$PKG_URL" | grep -o 'https://[^ "#]*' | tail -n 1)"
    TMP=pypi_$RANDOM$RANDOM
    mkdir /tmp/$TMP/
    cd /tmp/$TMP
    wget "$ARCHIVE_URL"
}
