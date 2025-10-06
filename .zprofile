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
export SUDO_ASKPASS="$HOME/.local/bin/dm-password"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"
export XMONAD_CONFIG_DIR="$XDG_CONFIG_HOME/xmonad"

# XDG runtime directory
# ! [ -d "$HOME/.local/run/$(id -u)" ] && mkdir -p "$HOME/.local/run/$(id -u)"
# export XDG_RUNTIME_DIR="$HOME/.local/run/$(id -u)"

# Start graphical server on user's current tty if not already running.
echo "Select a window manager (WM) to start your system:
	 1  Dwm    - The suckless window manager, written in C
	 2  Qtile  - Python's window manager
	 3  Xmonad - WM written entirely in Haskell
	 *  Exit   - Enter the shell
"
printf "Choice: "
read -r wm
case "$wm" in
	1) export WM="dwm" && startx "$XINITRC" ;;
	2) export WM="qtile" && startx "$XINITRC" ;;
	3) export WM="xmonad" && startx "$XINITRC" ;;
	*) return ;; # Don't start any window manager and just return to shell.
esac
