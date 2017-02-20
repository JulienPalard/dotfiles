#!/usr/bin/env python3

from filecmp import cmpfiles
from difflib import unified_diff
from os.path import expanduser, join, isdir, exists
from shutil import copy2 as copy
from os import listdir
from itertools import chain


def maybe_copy_file(src, dest):
    if not exists(dest):
        if input("Copy {} [y,n]? ".format(src)).lower()[0] == 'y':
            copy(src, dest)
        return
    with open(src) as a, open(dest) as b:
        diff = unified_diff(list(a), list(b))
        print(''.join(list(diff)))
        if input("Copy [y,n]? ").lower()[0] == 'y':
            copy(src, dest)


def maybe_copy_dir(src, dest, common):
    matches, missmatches, errors = cmpfiles(src, dest, common, shallow=False)
    for match in sorted(matches):
        print(join(dest, match), '√')
    for missmatch in sorted(chain(missmatches, errors)):
        if isdir(missmatch):
            maybe_copy_dir(join(src, missmatch), join(dest, missmatch),
                           listdir(join(src, missmatch)))
        else:
            maybe_copy_file(missmatch, join(expanduser('~/'), missmatch))


if __name__ == '__main__':
    maybe_copy_dir(
        './',
        expanduser('~/'),
        set(listdir()) - set(('README.md', '.git', 'copy_to_home.py',
                              'install.sh')))
