#!/usr/bin/env python3

"""Interactively and recursively copy files, prompting user when file
differs, showing him diffs.
"""

from filecmp import cmpfiles
from difflib import unified_diff
from os.path import expanduser, join, isdir, exists, dirname
from os import listdir, makedirs
from shutil import copy2 as copy
from itertools import chain


def maybe_copy_file(src, dest):
    if not exists(dest):
        if input("Copy {} [y,n]? ".format(src)).lower()[0] == 'y':
            makedirs(dirname(dest), exist_ok=True)
            copy(src, dest)
        return
    with open(src) as a, open(dest) as b:
        diff = unified_diff(list(b), list(a))
        print(''.join(list(diff)))
        if input("Copy {} [y,n]? ".format(src)).lower()[0] == 'y':
            copy(src, dest)


def maybe_copy_dir(src, dest, common):
    matches, missmatches, errors = cmpfiles(src, dest, common, shallow=False)
    for match in sorted(matches):
        print(join(dest, match), '√')
    for missmatch in sorted(chain(missmatches, errors)):
        if isdir(join(src, missmatch)):
            maybe_copy_dir(join(src, missmatch), join(dest, missmatch),
                           listdir(join(src, missmatch)))
        else:
            maybe_copy_file(join(src, missmatch), join(dest, missmatch))


def parse_args():
    import argparse
    parser = argparse.ArgumentParser(description='Interactively copy files.')
    parser.add_argument('src')
    parser.add_argument('dest')
    parser.add_argument('--exclude', help='Exclude those files', nargs='*',
                        default=[])
    return parser.parse_args()


def main(src, dest, exclude):
    maybe_copy_dir(expanduser(src), expanduser(dest),
                   set(listdir(src)) - set(exclude))


if __name__ == '__main__':
    main(**vars(parse_args()))
