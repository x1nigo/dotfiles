autoload -U colors && colors
PS1="%B%F{red}%n%f%b on %B%F{cyan}%m%f %F{blue}%~%f
%F{white}->%f%b "
setopt autocd
stty stop undef

# [ $(command -v colorscript) ] && colorscript -r

# Load aliases and shortcuts.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && . "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && . "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"

zle-line-init() {
    zle -K viins
    echo -ne "\e[1 q" # This uses the block version of the cursor.
}
zle -N zle-line-init

# Load syntax highlighting; should be last.
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh 2>/dev/null
