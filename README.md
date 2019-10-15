# Install a new Debian

On a laptop, I typically use
https://cdimage.debian.org/images/unofficial/non-free/images-including-firmware/
to get the Wi-Fi firmwares.


# Pin default release

    echo 'APT::Default-Release "bullseye";' > /etc/apt/apt.conf.d/00default

Then add sid (for firefox, at least):

    deb https://deb.debian.org/debian sid main
    deb-src https://deb.debian.org/debian sid main


# Packages I typically use

```
apt-get install arandr ecryptfs-utils emacs feh firefox fonts-liberation fonts-symbola git git-completion i3 i3lock-fancy jq network-manager-gnome rxvt-unicode unifont wget xautolock xorg
```

and `update-alternatives --configure x-terminal-emulator` to choose urxvt.

# Install my dotfiles

    sh <(curl -s https://raw.githubusercontent.com/JulienPalard/dotfiles/master/install.sh)


## Packages I may need

```
apt-get install firmware-iwlwifi
apt-get install firmware-nonfree
apt-get install laptop-mode-tools
apt-get install opensc opensc-pkcs11  # For yubikey
```

# What I typically do on a new laptop

 - Change grub sleep time in `/etc/default/grub` (and run `update-grub`)
 - Set `Option "Tapping" "on"` for touchpad in `/usr/share/X11/xorg.conf.d/40-libinput.conf`
 - (May have to change button map, like `xinput set-button-map 12 1 2 2 4 5 6 7`, use `xev` from `x11-utils` to see button ids).
 - https://wiki.debian.org/TransparentEncryptionForHomeFolder
 - Storage=volatile dans /etc/systemd/journald.conf


# Firefox extensions

 - [Privacy Badger](https://chrome.google.com/webstore/detail/privacy-badger/pkehgijcmpdhfbdbbnkijodmdjhbjlgp?utm_source=chrome-app-launcher-info-dialog)
