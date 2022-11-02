#
# X1nigo's config for the `Z Shell`.
#

# Prompt Variables
root='%B%F{blue}[%f%F{red}%n%f%F{yellow}@%f%F{cyan}%m%f %F{magenta}%~%f%F{blue}]%f%F{red}#%f%b '
user='%B%F{red}[%f%F{magenta}%n%f%F{green}@%f%F{yellow}%m%f %F{blue}%~%f%F{red}]%f%F{cyan}$%f%b '

# Prompt Function
[ $(whoami) = "root" ] && PROMPT=$root || PROMPT=$user

# Settings (auto cd into dir & stop ctrl-s from freezing terminal)
setopt autocd
stty stop undef

# Syntax Colors
alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"
alias ip="ip --color=auto"
alias pacman="pacman --color always"
alias diff="diff --color=auto"
alias bat="bat --color=always --style=full"

# Commands
alias merge="xrdb -merge $HOME/.Xresources"
alias lf="lfrun"
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias b="bluetoothctl"
alias hs="hugo server --noHTTPCache"
alias s="systemctl"
alias startx="startx $XINITRC"
alias smci="sudo make clean install"

# Syntax Highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
