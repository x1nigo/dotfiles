#!/bin/sh

# This is used to adjust for the dots per inch.
xrandr --dpi 96
xsetroot -solid '#121212' &
# setwp &

autostart="dunst sl-bar pipewire transmission-daemon"

for program in $autostart; do
	pgrep -x "$program" || "$program" &
done >/dev/null 2>&1

# Decrease key repeat delay to 300ms and increase key repeat rate to 50ms per second.
xset r rate 300 50

# Merge xresources
xrdb $HOME/.Xresources

# Suckless' dynamic window manager
while :; do
	ssh-agent dwm
done
