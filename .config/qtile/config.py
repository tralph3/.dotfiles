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

from libqtile.config import Key, Group, Drag, Click, Match, Screen
from libqtile import bar, layout, widget, lazy, hook
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from libqtile.dgroups import simple_key_binder
from typing import List

# vars
MARGIN = 5
BORDER_WIDTH = 2
FOCUS_COLOR = "#51A3A3"
NORMAL_COLOR = "#2B2D43"
BORDER_COLOR = "#202132"
FONT="UbuntuMono Nerd Font Mono"
ICON_SIZE = 25
FONT_SIZE = 15
mod = "mod4"

terminal = guess_terminal()

keys = [
    # Switch windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),

    # Move windows
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),

    # Resize windows
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

    # Close window
    Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),

    # Switch layouts
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),

    # Volume
    Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer set Master 5%+")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("amixer set Master 5%-")),

    # Terminal
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),

    # Rofi
    Key([mod], "r", lazy.spawn("rofi -show run"), desc="Spawn a rofi Window"),

    # Firefox
    Key([mod], "i", lazy.spawn("firefox"), desc="Open Firefox"),
    Key([mod, "shift"], "i", lazy.spawn("firefox --private-window"), desc="Open Firefox in Incognito mode"),

    # Flameshot
    Key([], "Print", lazy.spawn("flameshot gui"), desc="Take screenshot"),

    # PcManFM
    Key([mod], "f", lazy.spawn("pcmanfm"), desc="File browser"),

    # Qtile
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
]

groups = [
    Group(""), # Terminal
    Group("", matches=[Match("firefox")]), # Web browser
    Group(""), # Coding
    Group(""), # File browser
    Group("", layout="Floating"), # Gaming stuff
    Group("", matches=[Match("discord")]), # Discord/Communication
    Group(""), # Music
    Group(""), # Image editing
    Group(""), # Anything else
]

# Binds "mod + X" to switch between grops
dgroups_key_binder = simple_key_binder(mod)

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

layout_conf = {
    "border_focus": FOCUS_COLOR,
    "border_normal": NORMAL_COLOR,
    "border_width": BORDER_WIDTH,
    "margin": MARGIN
}

layouts = [
    layout.Columns(**layout_conf),
    layout.Max(**layout_conf),
    layout.Floating(**layout_conf),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="sans",
    fontsize=12,
    padding=3,
)

extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    font=FONT,
                    fontsize=ICON_SIZE,
                    border_width=1,
                    disable_drag=True,
                    margin=3,
                    highlight_method="line",
                ),

                widget.WindowName(
                    font=FONT,
                    fontsize=FONT_SIZE,
                ),

                widget.Systray(
                    font=FONT,
                    fontsize=FONT_SIZE,
                ),

                widget.Clock(
                    format="%d/%m/%Y\n%a %I:%M %p",
                    font=FONT,
                    fontsize=FONT_SIZE / 1.5,
                ),
            ],
            26,  # size
            border_width=[0, 0, 2, 0],
            background=NORMAL_COLOR,
            border_color=[
                BORDER_COLOR,
                BORDER_COLOR,
                BORDER_COLOR,
                BORDER_COLOR
            ]
        ),
    ),
]


dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"

# Autostart
@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.run([home])

