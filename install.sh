#!/bin/sh

git clone -q git@github.com:JulienPalard/dotfiles.git /tmp/.dotfiles/ &&
    ./copy_to_home.py /tmp/.dotfiles/ ~/ --exclude README.md .git copy.py install.sh &&
    rm -fr /tmp/.dotfiles/
