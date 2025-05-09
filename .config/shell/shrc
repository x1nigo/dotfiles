#!/bin/sh

# This is the shrc file; To use this with other shells, just
# create a symlink with the proper renamed file.
# (e.g. bashrc / kshrc)
# ... or just copy the text.

# Get hostname (sometimes the `hostname` command is not present)
host=$(cat /etc/hostname)
[ -z $host ] && host=localhost

# Know your shell.
case "$0" in
	"/bin/sh") PS1='$(pwd | sed "s|$HOME|~|") \$ ' ;;
	*) PS1='\[\e[1;31m[\[\e[1;33m\u\[\e[1;32m@\[\e[1;34m\h \[\e[1;35m$(pwd | sed "s|$HOME|~|")\[\e[1;31m]\e[m\]\$ ' ;;
esac

# Load aliases and shortcuts.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && . "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && . "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"

# Edit files under `Home` directory.
se () {
    clear
    echo "Edit file:"
    file="$(find $HOME -type f | sed "s|$HOME|~|" | fzf --height=20% --reverse --no-separator)"
    file="$(echo "$file" | sed "s|~|$HOME|")"
    backtrack="$(pwd)"
    dir="${file%/*}"
    [ -f "$file" ] && cd "$dir" && $EDITOR "$file" && cd "$backtrack"
    clear
}
