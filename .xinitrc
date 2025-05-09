#!/bin/sh

xset r rate 300 50
xrandr --dpi 96
setwp &
xrdb $HOME/.Xresources

autostart="dunst xcompmgr dwmblocks pipewire transmission-daemon"
for program in $autostart; do
	pgrep -x "$program" || "$program" &
done >/dev/null 2>&1

while true; do
	ssh-agent dwm
done
