PS1='$USER: ${PWD}
\$ '

set -o vi
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && . "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && . "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"

ff () {
	file="$(find $HOME -type f | fzf --height=40% --layout=reverse --prompt='EDIT FILE: ')"
	backtrack="$(pwd)"
	dir="${file%/*}"

	[ -f "$file" ] && cd "$dir" && $EDITOR "$file" && cd "$backtrack"
}

sff () {
	sf
	cd "$(cat ${XDG_CACHE_HOME:-$HOME/.cache}/sf/.sf_d)"
}

extract () {
	if [ -f "$1" ]; then
		case $1 in
			*.tar.bz2)	tar -xjf	$1	;;
			*.tar.gz)	tar -xzf	$1	;;
			*.bz2)		bunzip2		$1	;;
			*.gz)		gunzip		$1	;;
			*.tar|*.tar.xz)	tar -xf		$1	;;
			*.tbz2)		tar -xjf	$1	;;
			*.zip)		unzip		$1	;;
			*.Z)		uncompress	$1	;;
			*.7z)		7z x		$1	;;
		esac
	else
		echo "'$1' is not a valid file."
	fi
}
