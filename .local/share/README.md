---
title: "x1nigo's README file"
date: 2026-02-26
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
|--------------------| -----------------------------------------|
| Mod + Return [^1]  | terminal                                 |
| Mod + W            | browser                                  |
| Mod + D            | launcher                                 |
| Mod + Control + F  | toggle fullscreen                        |
| Mod + q            | close [kill] windows                     |
| Mod + Backspace    | system options (shutdown, restart, etc.) |
| Mod + F11          | restart dwmblocks (statusbar)            |
| Mod + F12          | restart the window manager (dwm)         |

[^1]: The "Mod" key is basically your Windows/Command key. But you
can change it if you want in the config.h file.

# Fonts and other characters
It is recommended to place fonts inside the folder:

```
~/.local/share/fonts
```

To preserve the priority of fonts, edit the ~/.config/fontconfig/fonts.conf
file. Note the font-families: Serif, Sans-serif, Sans, and Monospace.

# How to compile
Cd into any suckless main folder and run "sudo make clean install".
Or just "sudo make install".
Press Mod + F12 to restart the window manager and see changes.
