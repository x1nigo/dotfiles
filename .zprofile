#!/bin/sh

# Add `~/.local/bin` to $PATH
export PATH="$PATH:/$HOME/.local/bin:$HOME/.local/bin/modules"

# Go through a list of browsers and export that which is installed (first in line).
browsers="zen-browser librewolf firefox chromium"
for item in $browsers; do
	command -v "$item" && export BROWSER="$item" && break
done
# General programs and commands
[ $(command -v nvim) ] && editor="nvim" || editor="vim"
export EDITOR="$editor"
export VISUAL="$editor"
export TERMINAL="st"

# XDG base directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export XINITRC="$XDG_CONFIG_HOME/x11/xinitrc"
export SUDO_ASKPASS="$HOME/.local/bin/dmenupass"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"
export XMONAD_CONFIG_DIR="$XDG_CONFIG_HOME/xmonad"

# XDG runtime directory
# ! [ -d "$HOME/.local/run/$(id -u)" ] && mkdir -p "$HOME/.local/run/$(id -u)"
# export XDG_RUNTIME_DIR="$HOME/.local/run/$(id -u)"

# Start graphical server on user's current tty if not already running.
echo "Select a window manager to start your system:
	[1] Dwm, the suckless (dynamic) window manager
	[2] Qtile, Python's window manager
	[3] XMonad, written entirely in Haskell
	[*] Exit script; enter shell
"
printf "Choice: "
read -r wm
case "$wm" in
	1) export WM="Dwm" && startx "$XINITRC" ;;
	2) export WM="Qtile" && startx "$XINITRC" ;;
	3) export WM="XMonad" && startx "$XINITRC" ;;
	*) return ;; # Don't start any window manager and just return to shell.
esac
