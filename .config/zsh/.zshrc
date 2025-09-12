autoload -U colors && colors
cat "${XDG_DATA_HOME:-$HOME/.local/share}/prompt_art.txt"
PS1="%B%F{cyan}%n%f%b on %B%F{magenta}%m%f %F{blue}%~%f
%F{cyan}->%f%b "
setopt autocd
stty stop undef

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
