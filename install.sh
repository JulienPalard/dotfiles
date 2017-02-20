#!/bin/sh

git clone -q git@github.com:JulienPalard/dotfiles.git /tmp/.dotfiles/ &&
    cd /tmp/.dotfiles &&
    ./copy_to_home.py &&
    rm -fr /tmp/.dotfiles/
