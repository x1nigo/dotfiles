# User prompt
autoload -U colors && colors
PROMPT="%B%F{cyan}%n%f on %F{magenta}%m%f %F{blue}%~%f
%F{cyan}>%f%b "

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

# Syntax Highlighting for the shell.
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh 2>/dev/null
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh 2>/dev/null
