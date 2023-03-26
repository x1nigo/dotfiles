# Prompt Variables
root='%B%F{red}[%m%f %F{magenta}%1~%f%F{red}]#%f%b '
user='%B%F{red}[%f%F{yellow}%n%f%F{green}@%f%F{blue}%m%f %F{magenta}%~%f%F{red}]%f%F{cyan}$%f%b '

# Prompt Function
[ $(whoami) = "root" ] && PROMPT=$root || PROMPT=$user

# Settings (auto cd into dir & stop ctrl-s from freezing terminal)
setopt autocd
stty stop undef
set -o vi

# Syntax Colors
alias \
	ls="ls --color=auto" \
	grep="grep --color=auto" \
	egrep="egrep --color=auto" \
	fgrep="fgrep --color=auto" \
	ip="ip --color=auto" \
	pacman="pacman --color always" \
	diff="diff --color=auto" \
	bat="bat --color=always --style=full"

# Commands
alias \
	merge="xrdb -merge $HOME/.Xresources" \
	lf="lfrun" \
	rm="rm -i" \
	cp="cp -i" \
	mv="mv -i" \
	p="pacman" \
	b="bluetoothctl" \
	hs="hugo server --noHTTPCache" \
	s="systemctl" \
	startx="startx $XINITRC" \
	rc="sudo make clean install" \
        bk="cd $HOME/dox/books/" \
        m="neomutt" \
	t="transmission-remote"

# Syntax Highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
