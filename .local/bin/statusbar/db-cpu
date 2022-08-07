#! /bin/sh

# Install the systat package first.

cpu=$(mpstat | awk '/all/ { print int(100-$13) }') && icon="📀"

case $BUTTON in
	1) st -e gotop ;;
	3) st -e vim ~/.config/dwmblocks/modules/cpu.sh ;;
	5) st -e vim ~/.config/dwmblocks/blocks.h ;;
esac

printf "%s %s%%\n" "$icon" "$cpu"
