#! /bin/sh

case $BUTTON in
	3) st -e vim ~/.config/dwmblocks/modules/nettraffic.sh ;;
	5) st -e vim ~/.config/dwmblocks/blocks.h ;;
esac

if grep -x up /sys/class/net/wl*/operstate >/dev/null 2>&1 ; then
	rx=$(cat /sys/class/net/wl*/statistics/rx_bytes)
	tx=$(cat /sys/class/net/wl*/statistics/tx_bytes)
elif grep -x up /sys/class/net/e*/operstate >/dev/null 2>&1 ; then
	rx=$(cat /sys/class/net/e*/statistics/rx_bytes)
	tx=$(cat /sys/class/net/e*/statistics/tx_bytes)
else
	rx=0; tx=0 && exit
fi

if [ $(ls /tmp | grep 'rx.txt') ]; then
	received=$(( $rx - $(cat /tmp/rx.txt) )) && echo $rx > /tmp/rx.txt
else
	received="$rx" && echo $rx > /tmp/rx.txt
fi

if [ $(ls /tmp | grep 'tx.txt') ]; then
	transmitted=$(( tx - $(cat /tmp/tx.txt) )) && echo $tx > /tmp/tx.txt
else
	transmitted="$tx" && echo $tx > /tmp/tx.txt
fi

printf "%s%6sB/s %s%6sB/s\n" "🔻" "$(numfmt --to=iec-i $received)" "🔺" "$(numfmt --to=iec-i $transmitted)"
