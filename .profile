#!/bin/sh

# Add `~/.local/bin` to $PATH
export PATH="$PATH:/$HOME/.local/bin:$HOME/.local/bin/statusbar"

# Default programs
export EDITOR="vim"
export TERMINAL="st"
export BROWSER="firefox"
export VISUAL="vim"

# XDG base directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# Source the ENV variable
export ENV="$HOME/.shrc"

# Start the user interface upon login
startx
