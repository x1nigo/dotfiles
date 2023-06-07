# Prompt Variables
root='%B%F{red}[%m%f %F{cyan}%1~%f%F{red}]#%f%b '
user='%B%F{red}[%f%F{yellow}%n%f%F{green}@%f%F{blue}%m%f %F{magenta}%~%f%F{red}]%f%F{white}$%f%b '

# Prompt Function
[ $(whoami) = "root" ] && PROMPT=$root || PROMPT=$user

# Settings (auto cd into dir & stop ctrl-s from freezing terminal)
setopt autocd
stty stop undef
set -o vi

sf () {
	file="$(find $HOME -type f | fzf --height=30 --layout=reverse)"
	backtrack="$(pwd)"
	dir="${file%/*}"

	[ -f "$file" ] && cd "$dir" && $EDITOR "$file" && cd "$backtrack"
	;}

# Verbose mode for some commands
alias \
	rm="rm -Iv" \
	cp="cp -iv" \
	mv="mv -iv"

# Colorize your commands
alias \
	ls="ls --color=auto --group-directories-first" \
	grep="grep --color=auto" \
	egrep="egrep --color=auto" \
	fgrep="fgrep --color=auto" \
	ip="ip --color=auto" \
	pacman="pacman --color always" \
	diff="diff --color=auto" \
	bat="bat --color=always --style=full"

# Commands that are too long
alias \
	merge="xrdb -merge $HOME/.config/x11/xresources" \
	lf="lfrun" \
	p="pacman" \
	sp="sudo pacman" \
	b="bluetoothctl" \
	hs="hugo server --noHTTPCache" \
	s="systemctl" \
	startx="startx $XINITRC" \
        m="neomutt" \
	t="transmission-remote" \
	bc="bc -ql" \
	rsync="rsync -rtvzP" \
	yt="yt-dlp" \
	yta="yt-dlp -x -f bestaudio/best" \
	z="zathura"

# Syntax Highlighting
source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh 2>/dev/null
