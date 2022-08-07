#! /bin/sh

mem=$(free | awk '/Mem:/ { print int($3 * 100 / $2) }') && icon="💾"

case $BUTTON in
	1) st -e gotop ;;
	3) st -e vim ~/.config/dwmblocks/modules/memory.sh ;;
	5) st -e vim ~/.config/dwmblocks/blocks.h ;;
esac

printf "%s %s%%\n" "$icon" "$mem"
