#!/bin/sh

# wget -qO- dotfiles.mandark.fr/all | sh

backup_directory="$(date +%Y-%m-%d-%H:%M:%S)"

mkdir -p ~/$backup_directory

for file in emacs bashrc Xresources gitconfig screenrc rc.xml
do
    case $file in
        rc.xml)
            if [ -d ~/.config/openbox/ ]
            then
                [ -f ~/.config/openbox/rc.xml ] && mv ~/.config/openbox/rc.xml ~/$backup_directory/
                wget -qO ~/.config/openbox/rc.xml mandark.fr/dotfiles/$file
            fi
            ;;
        *)
            [ -f ~/.$file ] && mv ~/.$file ~/$backup_directory/
            wget -qO ~/.$file mandark.fr/dotfiles/$file
            ;;
    esac
done

echo "Your old files are in ~/$backup_directory"
