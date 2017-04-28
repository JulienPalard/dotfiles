#!/bin/sh

rm -fr /tmp/.dotfiles/
git clone -q https://github.com/JulienPalard/dotfiles.git /tmp/.dotfiles/ &&
    /tmp/.dotfiles/interactive_copy.py /tmp/.dotfiles/ ~/ --exclude README.md .git interactive_copy.py install.sh __pycache__ &&
    rm -fr /tmp/.dotfiles/
