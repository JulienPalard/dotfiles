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
export LC_CTYPE=fr_FR.UTF-8
export LC_TIME=fr_FR.UTF-8
export LC_NUMERIC=fr_FR.UTF-8
export LC_MONETARY=fr_FR.UTF-8
export LC_MESSAGES=C
export LC_PAPER=fr_FR.UTF-8
export LC_NAME=fr_FR.UTF-8
export LC_ADDRESS=fr_FR.UTF-8
export LC_TELEPHONE=fr_FR.UTF-8
export LC_MEASUREMENT=fr_FR.UTF-8
export LC_IDENTIFICATION=fr_FR.UTF-8

# Set background color
xsetroot -solid "#3A3A3A"

# Enable core dumps in case something goes wrong
ulimit -c unlimited

# Start i3
feh --bg-fill /home/mandark/meltyLab2.jpg &
xautolock -time 3 -locker i3lock-fancy -detectsleep &
# urxvtd -q -o -f
exec /usr/bin/i3
