---
title: "x1nigo's README file"
date: 2026-10-30
---

# Introduction
This 'readme' file is meant to be direct and concise. Each section
has been labeled by its particular heading title.

# Keybinds
They can be found within my dwm config.h file:

```
~/.local/src/dwm/config.h
```

For convenience, the most prominent ones are the following:

| Keybind/s          | Action/Program                           |
|--------------------|------------------------------------------|
| Mod + Return [^1]  | terminal                                 |
| Mod + W            | browser                                  |
| Mod + D            | launcher                                 |
| Mod + Control + F  | toggle fullscreen                        |
| Mod + q            | close [kill] windows                     |
| Mod + x            | change wallpaper [^2]                    |
| Mod + Backspace    | system options (shutdown, restart, etc.) |
| Mod + F11          | restart dwmblocks (statusbar)            |
| Mod + F12          | restart the window manager (dwm)         |

[^1]: The "Mod" key is basically your Windows/Command key. But you
can change it if you want in the config.h file.

[^2]: Check out `~/.local/share/backgrounds/` for a list of wallpapers.
You can also start adding your own there.

# Window layout
Dwm operates using a "stack" layout, regardless of its form. Every new window
goes on top of the stack, becoming the new master window. However, I've
patched dwm to only spawn new windows on top of the stack, without affecting
the current master window.

# Fonts and other characters
It is recommended to place fonts inside the folder:

```
~/.local/share/fonts/
```

To preserve the priority of fonts, edit the ~/.config/fontconfig/fonts.conf
file. Note the font-families: Serif, Sans-serif, Sans, and Monospace.

# How to compile
Cd into any suckless main folder and run "sudo make clean install".
Or just "sudo make install".
Press Mod + F12 to restart the window manager and see changes.
