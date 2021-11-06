# Install a new Debian

On a laptop, I typically use
https://cdimage.debian.org/images/unofficial/non-free/images-including-firmware/
to get the Wi-Fi firmwares.


# Pin default release

    echo 'APT::Default-Release "bookworm";' > /etc/apt/apt.conf.d/00default

Then add sid (for firefox, at least):

    deb https://deb.debian.org/debian sid main
    deb-src https://deb.debian.org/debian sid main


# Packages I typically use

```bash
apt install \
    bash-completion \
    dnsutils \
    ecryptfs-utils \
    emacs-gtk \
    exuberant-ctags \
    feh \
    firefox \
    fonts-liberation \
    fonts-symbola \
    git \
    git-completion \
    gnome \
    jq \
    keepassxc \
    libpq-dev \
    py3status \
    python-openssl \
    python3-venv \
    tk-dev \
    unifont
```

and to build Python:

```bash
apt install \
    make \
    build-essential \
    libssl-dev \
    zlib1g-dev \
    libbz2-dev \
    libreadline-dev \
    libsqlite3-dev \
    wget \
    curl \
    llvm \
    libncursesw5-dev \
    xz-utils \
    tk-dev \
    libxml2-dev \
    libxmlsec1-dev \
    libffi-dev \
    liblzma-dev
```


# Install my dotfiles

    sh <(curl -s https://raw.githubusercontent.com/JulienPalard/dotfiles/master/install.sh)


## Packages I may need

```
apt-get install firmware-iwlwifi
apt-get install firmware-nonfree
apt-get install tlp  # Optimize Laptop Battery Life
apt-get install opensc opensc-pkcs11  # For yubikey
```

With:

    echo'CPU_ENERGY_PERF_POLICY_ON_BAT=power > /etc/tlp.d/50-cpu.conf

# What I typically do on a new laptop

 - Change grub sleep time in `/etc/default/grub` (and run `update-grub`)
 - Set `Option "Tapping" "on"` for touchpad in `/usr/share/X11/xorg.conf.d/40-libinput.conf`
 - (May have to change button map, like `xinput set-button-map 12 1 2 2 4 5 6 7`, use `xev` from `x11-utils` to see button ids).
 - https://wiki.debian.org/TransparentEncryptionForHomeFolder
 - Storage=volatile dans /etc/systemd/journald.conf


# Firefox extensions

 - [Privacy Badger](https://chrome.google.com/webstore/detail/privacy-badger/pkehgijcmpdhfbdbbnkijodmdjhbjlgp?utm_source=chrome-app-launcher-info-dialog)


# Crontab

```text
PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
DISPLAY=:0

# m h  dom mon dow   command
0 */4 * * * curl https://apod.nasa.gov/apod/astropix.html | grep -o 'image/[^ ]*.jpg' | sed 's#^#https://apod.nasa.gov/apod/#' | head -n 1 | xargs wget -O $HOME/apod.jpg && feh --bg-fill $HOME/apod.jpg && convert -resize 1920x1080^ $HOME/apod.jpg $HOME/apod.png
