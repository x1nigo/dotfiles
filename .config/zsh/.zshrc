# User prompt
autoload -U colors && colors
PROMPT="%B%F{red}%n%f: [%~]%b "

# Settings (auto cd into dir & stop ctrl-s from freezing terminal)
setopt autocd
stty stop undef
set -o vi

# Basic auto/tab complete.
autoload -Uz compinit
zstyle ':completion:*' menu select
compinit
_comp_options+=(globdots)    # To include hidden files.

# Load aliases and shortcuts.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"

# Find files under `Home` directory.
ff () {
	file="$(find $HOME -type f | fzf --height=40% --layout=reverse --prompt='EDIT FILE: ')"
	backtrack="$(pwd)"
	dir="${file%/*}"

	[ -f "$file" ] && cd "$dir" && $EDITOR "$file" && cd "$backtrack"
	;}

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

# Syntax Highlighting for the shell.
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions-plugin.zsh 2>/dev/null
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh 2>/dev/null
