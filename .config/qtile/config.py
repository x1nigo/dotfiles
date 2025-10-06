# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import os
import subprocess

import libqtile.resources
from libqtile import bar, layout, qtile, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy

# `qtile-extras` from the AUR
from qtile_extras import widget
from qtile_extras.widget.decorations import BorderDecoration

import colors
colors = colors.Dark

mod = "mod4"
terminal = "st" # guess_terminal()
browser = "brave" # librewolf, firefox, etc.
filemanager = "lfup"
volumecontrols = "pulsemixer"
dmenu_command = "dmenu_run -p 'RUN:' -l 6 -g 8"

# @hook.subscribe.startup_once
# def autostart():
#     home = os.path.expanduser('~/.config/qtile/autostart.sh')
#     subprocess.call(home)

keys = [
    # General programs and other scripts
    Key([mod], "Return", lazy.spawn(terminal), desc="Spawn the terminal"),
    Key([mod, "shift"], "Return", lazy.spawn(terminal + " -n termfloat"), desc="Spawn a floating terminal"), # Make sure to set the proper float rules for this to work.
    Key([mod], "w", lazy.spawn(browser), desc="Launch the browser"),
    Key([mod], "r", lazy.spawn("{} -e {}" .format(terminal, filemanager)), desc="Spawn the file manager"),
    Key([mod], "d", lazy.spawn(dmenu_command), desc="Launch a program"),
    Key([mod], "b", lazy.spawn("dm-bookmark"), desc="Bookmark the highlighted text"),
    Key([mod], "v", lazy.spawn("dm-videos"), desc="Watch a video through your media player"),
    Key([mod], "q", lazy.window.kill(), desc="Exit a window"),
    Key([mod], "x", lazy.spawn("dm-wallpaper -d"), desc="Select a desktop wallpaper/background"),
    Key([mod, "shift"], "x", lazy.spawn("dm-wallpaper -x"), desc="Remove the current desktop wallpaper/background"),
    Key([mod, "shift"], "Space", lazy.window.toggle_floating(), desc="Toggle the floating status of a window"),
    Key([mod], "apostrophe", lazy.spawn(terminal + " -n termfloat -f monospace:size=16 -g 50x20 -e bc -lq"), desc="Use a terminal-based calculator"),
    Key([mod], "Insert", lazy.spawn("dm-insert"), desc="Insert one of your saved bookmarks"),
    Key([mod], "grave", lazy.spawn("dm-emoji"), desc="Place selected emoji in your clipboard"),
    Key([mod], "u", lazy.spawn("dm-unicode"), desc="Copy selected unicode character"),
    Key([mod], "BackSpace", lazy.spawn("dm-system"), desc="The system menu"),
    # Media commands for both audio and backlight
    # This current setup uses pipewire and brightnessctl
    Key([], "XF86AudioMute", lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"), desc="Toggle the volume's mute option"),
    Key([], "XF86AudioMicMute", lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"), desc="Toggle the mic's mute option"),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"), desc="Raise the volume"),
    Key([], "XF86AudioLowerVolume", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"), desc="Lower the volume"),
    Key([], "XF86MonBrightnessUp", lazy.spawn("brightnessctl s 5%+"), desc="Raise the screen brightness"),
    Key([], "XF86MonBrightnessDown", lazy.spawn("brightnessctl s 5%-"), desc="Lower the screen brightness"),
    # Other commands based on the functions keys
    Key([mod], "F1", lazy.spawn("readme"), desc="Read the README file"),
    Key([mod], "F2", lazy.spawn("dm-fonts"), desc="Select a default font"),
    Key([mod], "F3", lazy.spawn("{} -e {}" .format(terminal, volumecontrols)), desc="Spawn the audio mixer"),
    Key([mod], "F4", lazy.spawn("dm-display"), desc="Select a display option"),
    Key([mod], "F12", lazy.reload_config(), desc="Reload the config"),
    Key([], "Print", lazy.spawn("dm-printscreen"), desc="Take a screenshot"),

    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    # Key([mod], "f", lazy.window.toggle_fullscreen()),
    Key([mod], "f", lazy.window.toggle_fullscreen(), desc='toggle fullscreen'),
    Key([mod], "space", lazy.window.move_to_top(), desc="Move window to top"),
]

# Add key bindings to switch VTs in Wayland.
# We can't check qtile.core.name in default config as it is loaded before qtile is started
# We therefore defer the check until the key binding is run by using .when(func=...)
# for vt in range(1, 8):
#     keys.append(
#         Key(
#             ["control", "mod1"],
#             f"f{vt}",
#             lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == "wayland"),
#             desc=f"Switch to VT{vt}",
#         )
#     )

# groups = [Group(i) for i in "123456789"]
groups = []
group_names = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

# Edit these to indicate new labels for workspaces
group_labels = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
# group_labels = [" www ", " dev ", " doc ", " vid ", " pix ", " mus ", " vbox ", " art ", " sys "]

for i in range(len(group_names)):
    groups.append(
        Group(
            name=group_names[i],
            label=group_labels[i],
            )
        )

for i in groups:
    keys.extend(
        [
            # mod + group number = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc=f"Switch to group {i.name}",
            ),
            # mod + shift + group number = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=False),
                desc=f"Switch to & move focused window to group {i.name}",
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod + shift + group number = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

my_layout = {
    "border_width": 2,
    "margin": 16,
    "border_focus": "#57d7f7",
    "border_normal": "#282828",
    }

separator_values = {
    "size_percent": 50,
    "foreground": "#373737",
    }

layouts = [
    layout.MonadTall(**my_layout),
    layout.Max(),
    layout.Bsp(**my_layout),
    layout.Matrix(**my_layout),
    # layout.Columns(),
    # layout.Tile(),
    # layout.Stack(num_stacks=2),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

floats_kept_above = True
floating_layout = layout.Floating(
    border_width = 3,
    border_focus = "#570028",
    border_normal = "#282828",
    float_rules = [
        *layout.Floating.default_float_rules,
        Match(wm_class = "termfloat"),
        Match(wm_class = "Xmessage"),
        ]
)

widget_defaults = dict(
    # Qtile seems to favor sans fonts, preferrably not monospace.
    # It also looks better in bold, unlike other window managers.
    # font = "sans bold",
    font = "Ubuntu Bold", # Make sure the font is available in the first place. Use `fc-list` to see.
    foreground = colors[7],
    fontsize = 12,
    padding = 7,
    margin = 5,
)
extension_defaults = widget_defaults.copy()

# logo = os.path.join(os.path.dirname(libqtile.resources.__file__), "logo.png")
screens = [
    Screen(
        top=bar.Bar(
            [
                # The margin really depends on what kind of image is rendered.
                widget.Image(filename = "~/.config/qtile/CB2OS-Logo.png", margin = 0),
                widget.Sep(**separator_values),
                widget.GroupBox(
                    highlight_method = "line", # block, text, etc.
                    highlight_color = colors[9],
                    active = colors[3],
                    inactive = colors[8],
                    borderwidth = 3,
                    block_highlight_text_color = colors[6],
                    this_current_screen_border = colors[5],
                    padding = 2,
                    ),
                widget.Sep(**separator_values),
                widget.CurrentLayout(
                    mode = "both",
                    icon_first = True,
                    scale = 0.7,
                    foreground = colors[1],
                    ),
                #                widget.Sep(**separator_values),
                #                widget.LaunchBar(
                #                    progs = [("🦁", "brave", "Browser"), # librewolf, firefox, chromium, etc.
                #                        ("🚀", "st", "The simple terminal"),
                #                        ("⌨️", "libreoffice", "Libre Office"),
                #                        ("🌏", "st -e nmtui", "Network Manager"),
                #                        ("🎸", "st -e ncmpcpp", "Music Player"),
                #                        ],
                #                    padding = 5,
                #                    ),
                widget.Sep(**separator_values),
                widget.WindowName(
                    fmt = "{}",
                    foreground = colors[6],
                    max_chars = 70,
                    # empty_group_string = "~",
                    ),
                widget.GenPollCommand(
                    cmd = ["uname", "-r"],
                    fmt = "  {}",
                    foreground = colors[8],
                    update_interval = 360,
                    decorations = [
                        BorderDecoration(
                            border_width = [0, 0, 2, 0],
                            colour = colors[8],
                            padding_x = 5,
                            ),
                        ],
                    ),
                widget.Backlight(
                    backlight_name = "intel_backlight",
                    fmt = "󰛨   Scr: {}",
                    foreground = colors[4],
                    update_interval = 6,
                    decorations = [
                        BorderDecoration(
                            border_width = [0, 0, 2, 0],
                            colour = colors[4],
                            padding_x = 5,
                            ),
                        ],
                    ),
                widget.CPU(
                    fmt = "   Cpu: {}",
                    format = "{load_percent}%",
                    foreground = colors[6],
                    update_interval = 30,
                    decorations = [
                        BorderDecoration(
                            border_width = [0, 0, 2, 0],
                            colour = colors[6],
                            padding_x = 5,
                            ),
                        ],
                    ),
                widget.Memory(
                    fmt = "   Mem: {}",
                    format = "{MemUsed:.0f}{mm} ({MemPercent:.0f}%)",
                    foreground = colors[1],
                    update_interval = 30,
                    decorations = [
                        BorderDecoration(
                            border_width = [0, 0, 2, 0],
                            colour = colors[1],
                            padding_x = 5,
                            ),
                        ],
                    ),
                widget.Volume(
                    mute_format = "   Muted: {volume}%",
                    unmute_format = "   Vol: {volume}%",
                    foreground = colors[3],
                    update_interval = 1,
                    decorations = [
                        BorderDecoration(
                            border_width = [0, 0, 2, 0],
                            colour = colors[3],
                            padding_x = 5,
                            ),
                        ],
                    ),
                widget.Battery(
                    fmt = "{}",
                    format = "{char}  Bat: {percent:2.0%}",
                    discharge_char = "",
                    empty_char = "",
                    charge_char = "",
                    full_char = "",
                    full_short_text = "full",
                    not_charging_char = "!",
                    foreground = colors[2],
                    update_interval = 12,
                    decorations = [
                        BorderDecoration(
                            border_width = [0, 0, 2, 0],
                            colour = colors[2],
                            padding_x = 5,
                            ),
                        ],
                    ),
                widget.Systray(),
                widget.Clock(
                    format="󱎫  %a, %b %d, %Y - %I:%M %p",
                    foreground = colors[5],
                    update_interval = 5,
                    decorations = [
                        BorderDecoration(
                            border_width = [0, 0, 2, 0],
                            colour = colors[5],
                            padding_x = 5,
                            ),
                        ],
                     ),
            ],
            30, # Bar height
            background = colors[0],
            margin = [8, 8, 0, 8], # Orientation: N, E, S, W
            # border_width=[0, 2, 0, 2],  # Draw top and bottom borders
            # border_color=["#000000", "#f74747", "#000000", "#ff8747"]
        ),
#         wallpaper=logo,
#         wallpaper_mode="center",
        # You can uncomment this variable if you see that on X11 floating resize/moving is laggy
        # By default we handle these events delayed to already improve performance, however your system might still be struggling
        # This variable is set to None (no cap) by default, but you can set it to 60 to indicate that you limit it to 60 events per second
        # x11_drag_polling_rate = 60,
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
auto_fullscreen = True
focus_on_window_activation = "smart"
focus_previous_on_window_remove = False
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# xcursor theme (string or None) and size (integer) for Wayland backend
wl_xcursor_theme = None
wl_xcursor_size = 24

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
