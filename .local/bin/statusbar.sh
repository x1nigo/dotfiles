#!/bin/sh

battery() {
	bat="$(apm -l)"
	echo "BAT: $bat%"
}

datetime() {
	date="$(date -u +'%Y %d %b %I:%M%p')"
	echo "$date"
}

while true; do
	xsetroot -name "$(battery) | $(datetime)"
	sleep 60
done
