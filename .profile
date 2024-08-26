#!/bin/sh

# Add `~/.local/bin` to $PATH
export PATH="$PATH:/$HOME/.local/bin:$HOME/.local/bin/statusbar"

# Default programs
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="firefox"
export VISUAL="nvim"

# XDG base directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# XDG runtime directory
! [ -d "$HOME/.local/run/$(id -u)" ] && mkdir -p "$HOME/.local/run/$(id -u)"
export XDG_RUNTIME_DIR="$HOME/.local/run/$(id -u)"

# Source the ENV variable
export ENV="$HOME/.shrc"

# Start the user interface upon login
startx
