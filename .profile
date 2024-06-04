#!/bin/sh

# Add `~/.local/bin` to $PATH
export PATH="${PATH}$(find $HOME/.local/bin -type d -printf ':%h/%f')"

# Default programs
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="firefox"
export VISUAL="nvim"
export MANPAGER="less -R --use-color -Dd+g -Du+b"

# XDG base directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# Source the kshrc file
export ENV="${XDG_CONFIG_HOME:-$HOME/.config}/ksh/kshrc"

# Start the graphical user interface
startx
