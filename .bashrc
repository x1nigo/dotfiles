#
# ~/.bashrc
#

# If not running interactively, don't do anything.
[[ $- != *i* ]] && return

# Autocd into any directory.
shopt -s autocd

# Use vi-mode.
set -o vi
bind -m vi-command '\C-l: clear-screen'
bind -m vi-insert '\C-l: clear-screen'

# Colors for your prompt.
BLACK='\[\e[1;30m\]'
RED='\[\e[1;31m\]'
GREEN='\[\e[1;32m\]'
YELLOW='\[\e[1;33m\]'
BLUE='\[\e[1;34m\]'
PURPLE='\[\e[1;35m\]'
CYAN='\[\e[1;36m\]'
WHITE='\[\e[1;37m\]'
# To reset the colors.
RESET='\[\e[0m\]'
BOLD_RESET='\[\e[1m\]'

# Your prompt.
PS1="${RED}\u${RESET}${BOLD_RESET}: [\w]${RESET} "

# Load aliases and shortcuts.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && . "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && . "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"

# Find files under `Home` directory.
ff () {
	file="$(find $HOME -type f | fzf --height=40% --layout=reverse --prompt='EDIT FILE: ')"
	backtrack="$(pwd)"
	dir="${file%/*}"

	[ -f "$file" ] && cd "$dir" && $EDITOR "$file" && cd "$backtrack"
	}

# Extract files based on their extension.
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
