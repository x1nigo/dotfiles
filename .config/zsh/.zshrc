#
# This is my zsh configuration.
#

# Prompt Variables
root='%B%F{blue}[%f%F{red}%n%f%F{yellow}@%f%F{cyan}%m%f %F{magenta}%1~%f%F{blue}]%f%F{red}#%f%b '
user='%B%F{red}[%f%F{blue}%n%f%F{green}@%f%F{yellow}%m%f %F{magenta}%1~%f%F{red}]%f%F{cyan}$%f%b '

# Prompt Function
[ $(whoami) = "root" ] && PROMPT=$root || PROMPT=$user

# Settings (auto cd into dir & stop ctrl-s from freezing terminal)
setopt autocd
stty stop undef

# Programs
export EDITOR="vim"
export TERMINAL="st"
export BROWSER="brave"
export VISUAL="vim"
export MANPAGER="less -R --use-color -Dd+g -Du+b"

# Syntax Colors
alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"
alias ip="ip --color=auto"
alias pacman="pacman --color always"
alias diff="diff --color=auto"

# Commands
alias merge="xrdb -merge $HOME/.Xresources"
alias lf="lfrun"
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias b="bluetoothctl"
alias hs="hugo server --noHTTPCache"

# Prompt Commands
# neofetch
# pfetch
# colorscript -r

# Add to PATH
PATH=$HOME/.scripts:$PATH

# Syntax Highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
