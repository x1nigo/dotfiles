#!/bin/sh

# Add `~/.local/bin` to $PATH
export PATH="$PATH:/home/chris/.local/bin"

# Default programs
export EDITOR="le"
export TERMINAL="st"
export BROWSER="firefox"
export VISUAL="vi"

# XDG base directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# Source the ENV variable
export ENV="$HOME/.shrc"

# Start the user interface upon login
startx
