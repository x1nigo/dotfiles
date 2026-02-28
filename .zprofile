#!/bin/sh

# Add `~/.local/bin` to $PATH
export PATH="$PATH:/$HOME/.local/bin:$HOME/.local/bin/modules"

# General programs and commands
[ $(command -v nvim) ] && editor="nvim" || editor="vim"
export EDITOR="$editor"
export VISUAL="$editor"
export BROWSER="firefox"
export TERMINAL="st"

# XDG base directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export XINITRC="$XDG_CONFIG_HOME/x11/xinitrc"
export XAUTHORITY="$HOME/.Xauthority"
export SUDO_ASKPASS="$HOME/.local/bin/dm-password"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"

# XDG runtime directory
# ! [ -d "$HOME/.local/run/$(id -u)" ] && mkdir -p "$HOME/.local/run/$(id -u)"
# export XDG_RUNTIME_DIR="$HOME/.local/run/$(id -u)"

startx "$XINITRC"
