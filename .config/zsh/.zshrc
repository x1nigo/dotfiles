# User prompt
autoload -U colors && colors
PS1="%B%F{red}[%f%(!.%F{red}%m%f.%F{yellow}%n%f)%(!..%F{green}@%f%F{blue}%m%f) %(!.%F{cyan}%1~%f.%F{magenta}%~%f)%F{red}]%f%(!.%F{red}#%f.%F{white}$%f)%b "

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
	file="$(find $HOME -type f | fzf --height=20% --layout=reverse --prompt='EDIT FILE: ')"
	backtrack="$(pwd)"
	dir="${file%/*}"

	[ -f "$file" ] && cd "$dir" && $EDITOR "$file" && cd "$backtrack"
	;}

# Change cursor shape for different vi modes.
function zle-keymap-select () {
	case $KEYMAP in
		vicmd) echo -ne '\e[1 q';;	# block
		viins|main) echo -ne '\e[5 q';;	# beam
	esac
}
zle -N zle-keymap-select
zle-line-init() {
	zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
	echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Syntax Highlighting for the shell.
source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh 2>/dev/null
