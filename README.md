# install

    sh <(curl -s https://raw.githubusercontent.com/JulienPalard/dotfiles/master/install.sh)


# Packages I typically use

```
apt-get install xorg i3 rxvt-unicode wicd wicd-curses feh fonts-liberation xautolock emacs25-nox git arandr firefox-esr ecryptfs-utils fonts-symbola unifont
```

## Packages I may need

```
apt-get install firmware-iwlwifi
apt-get install firmware-nonfree
apt-get install laptop-mode-tools
apt-get install opensc opensc-pkcs11  # For yubikey
```

# What I typically do on a new laptop

 - Change grub sleep time in `/etc/default/grub` (and run `update-grub`)
 - update-alternatives --config x-terminal-emulator
 - Set `Option "Tapping" "on"` for touchpad in `/usr/share/X11/xorg.conf.d/40-libinput.conf`
 - https://wiki.debian.org/TransparentEncryptionForHomeFolder
 - Storage=volatile dans /etc/systemd/journald.conf


# Firefox extensions

 - [Privacy Badger](https://chrome.google.com/webstore/detail/privacy-badger/pkehgijcmpdhfbdbbnkijodmdjhbjlgp?utm_source=chrome-app-launcher-info-dialog)
