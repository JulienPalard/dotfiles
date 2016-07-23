#!/bin/sh

rsync -bahv --exclude=.git --exclude=README.md --exclude=copy_to_home.sh ./ ~/
