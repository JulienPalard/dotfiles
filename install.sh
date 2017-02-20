#!/bin/sh

rm -fr /tmp/.dotfiles/
git clone -q git@github.com:JulienPalard/dotfiles.git /tmp/.dotfiles/ &&
    /tmp/.dotfiles/copy.py /tmp/.dotfiles/ ~/ --exclude README.md .git copy.py install.sh &&
    rm -fr /tmp/.dotfiles/
