# Disable DPMS turning off the screen
xset dpms force on
xset s off

# Disable bell
xset -b

# Enable zapping (C-A-<Bksp> kills X)
setxkbmap -option terminate:ctrl_alt_bksp

# Enable compose key on right alt
xmodmap -e "keysym Alt_R = Multi_key"

# Enforce correct locales from the beginning
unset LC_COLLATE
export LC_CTYPE=en_US.UTF-8
export LC_TIME=swedish
export LC_NUMERIC=en_US.UTF-8
export LC_MONETARY=en_US.UTF-8
export LC_MESSAGES=C
export LC_PAPER=en_US.UTF-8
export LC_NAME=en_US.UTF-8
export LC_ADDRESS=en_US.UTF-8
export LC_TELEPHONE=en_US.UTF-8
export LC_MEASUREMENT=en_US.UTF-8
export LC_IDENTIFICATION=en_US.UTF-8
export LC_ALL=

# Set background color
xsetroot -solid "#3A3A3A"

# Enable core dumps in case something goes wrong
ulimit -c unlimited

# Start i3
xautolock -time 3 -locker i3lock-fancy -detectsleep &

# urxvtd -q -o -f
exec /usr/bin/i3
