autoload -U colors && colors
PS1="%B%F{cyan}%n%f%b on %B%F{magenta}%m%f %F{blue}%~%f
%F{cyan}->%f%b "
setopt autocd
stty stop undef

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

zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Load syntax highlighting; should be last.
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh 2>/dev/null
