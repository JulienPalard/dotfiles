#!/bin/sh

rsync -bahv --exclude=.git --exclude=README.md --exclude=add.sh ./ ~/
