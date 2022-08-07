#! /bin/zsh

# Add `~/.local/bin` to $PATH
export PATH="${PATH}:$HOME/.local/bin"

# Default programs:
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="firefox"
export VISUAL="nvim"
export MANPAGER="less -R --use-color -Dg+g -Du+b"

# XDG base directories:
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATE_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# Zsh directory:
export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"

# Other(s):
export GTK2_RC_FILES="${XDG_CONFIG_HOME:-$HOME/.config}/gtk-2.0/gtkrc"
export SCREENRC="${XDG_CONFIG_HOME:-$HOME/.config}/screen/screenrc"
export LESSHISTFILE="-"
export GNUPGHOME="${XDG_CONFIG_HOME:-$HOME/.config}/gnupg"
export GOPATH="${XDG_DATE_HOME:-$HOME/.local/share}/go"
export PASSWORD_STORE_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/password-store"
