#! /bin/sh

dir="/sys/class/power_supply/BAT0"
bat=$(cat $dir/capacity) && stat=$(cat $dir/status)

case $stat in
        Discharging) icon="🔋" && [ $bat -le 10 ] && notify-send -t 5000 "❗ warning: low battery, please charge soon!" ;;
        Charging) icon="🔌" ;;
        Full) icon="⚡" ;;
esac

case $BUTTON in
        1) notify-send -t 5000 "$icon battery device is \`$stat\`!" ;;
        3) st -e vim ~/.config/dwmblocks/modules/battery.sh ;;
        5) st -e vim ~/.config/dwmblocks/blocks.h ;;
esac

printf "%s%s %s%%\n" "$warn" "$icon" "$bat"
