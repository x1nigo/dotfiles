#!/bin/zsh

# Add `~/.local/bin` to $PATH
export PATH="${PATH}:$(find $HOME/.local/bin -type d -printf ':%h/%f')"

# Default programs:
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="librewolf"
export VISUAL="nvim"
export MANPAGER="less -R --use-color -Dd+g -Du+b"

# XDG base directories:
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# Zsh directory:
export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"

# Other(s):
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch-config"
export GTK2_RC_FILES="${XDG_CONFIG_HOME:-$HOME/.config}/gtk-2.0/gtkrc"
export SCREENRC="${XDG_CONFIG_HOME:-$HOME/.config}/screen/screenrc"
export LESSHISTFILE="-"
export GNUPGHOME="${XDG_CONFIG_HOME:-$HOME/.config}/gnupg"
export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"
export MBSYNCRC="${XDG_CONFIG_HOME:-$HOME/.config}/mbsync/config"
export PASSWORD_STORE_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/password-store"
export XAUTHORITY="${XDG_CONFIG_HOME:-$HOME/.config}/x11/Xauthority"
export XINITRC="${XDG_CONFIG_HOME:-$HOME/.config}/x11/xinitrc"

# Start the graphical user environment:
startx $XINITRC
