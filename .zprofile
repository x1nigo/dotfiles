#!/bin/sh

# Add `~/.local/bin` to $PATH
export PATH="$PATH:/$HOME/.local/bin:$HOME/.local/bin/statusbar"
export RANGER_LOAD_DEFAULT_RC=FALSE

# Default programs
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="librewolf"
export VISUAL="nvim"

# XDG base directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export XINITRC="$XDG_CONFIG_HOME/x11/xinitrc"
export SUDO_ASKPASS="$HOME/.local/bin/dmenupass"

# XDG runtime directory
! [ -d "$HOME/.local/run/$(id -u)" ] && mkdir -p "$HOME/.local/run/$(id -u)"
export XDG_RUNTIME_DIR="$HOME/.local/run/$(id -u)"

# Start graphical server on user's current tty if not already running.
[ "$(tty)" = "/dev/tty1" ] && ! pidof -s Xorg >/dev/null 2>&1 && exec startx "$XINITRC"
