import os
import subprocess

from libqtile.config import Key, Group, Drag, Click, Match, Screen
from libqtile import bar, layout, widget, lazy, hook
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from libqtile.dgroups import simple_key_binder
from typing import List

# special dictionary that allows overriding values
class SettingsDict(dict):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def override(self, **kwargs):
        new_dict = self.copy()
        for key in kwargs:
            new_dict[key] = kwargs[key]
        return new_dict


#############
# VARIABLES #
#############
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


############
# KEYBINDS #
############
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
    Key([mod], "r", lazy.spawn("rofi -show drun"), desc="Spawn a rofi Window"),

    # Firefox
    Key([mod], "i", lazy.spawn("firefox"), desc="Open Firefox"),
    Key([mod, "shift"], "i", lazy.spawn("firefox --private-window"), desc="Open Firefox in Incognito mode"),

    # Flameshot
    Key([], "Print", lazy.spawn("flameshot gui"), desc="Take screenshot"),

    # Thunar
    Key([mod], "f", lazy.spawn("thunar"), desc="File browser"),

    # Qtile
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]


##########
# GROUPS #
##########
groups = [
    Group(""), # Terminal
    Group("", matches=[Match(wm_class="firefox")]), # Web browser
    Group(""), # Coding
    Group("", matches=[Match(wm_class="thunar")]), # File browser

    Group("", matches=[        # Gaming stuff
        Match(wm_class="Steam"),
        Match(wm_class="Lutris")
    ]),

    Group("", matches=[Match(wm_class="discord")]), # Discord/Communication

    Group("", matches=[        # Music
        Match(wm_class="Spotify"),
        Match(wm_class="Quodlibet")
    ]),

    Group(""), # Image editing
    Group(""), # Anything else
]

# Binds "mod + X" to switch between grops
dgroups_key_binder = simple_key_binder(mod)


###########
# LAYOUTS #
###########
layout_conf = SettingsDict(
    border_focus=FOCUS_COLOR,
    border_focus_stack=FOCUS_COLOR,
    border_normal=NORMAL_COLOR,
    border_normal_stack=NORMAL_COLOR,
    border_width=BORDER_WIDTH,
    margin=MARGIN,
    margin_on_single=MARGIN,
    border_on_single=True,
    fair=True   # Open new window in the side with the least ammount
)

layouts = [
    layout.Columns(**layout_conf),
    layout.Max(**layout_conf),
    layout.Floating(**layout_conf),
]

# Floating window config (different from the layout)
floating_layout = layout.Floating(
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(wm_class="flameshot"),  # Flameshot upload window
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ],
    **layout_conf,
)


###########
# WIDGETS #
###########
widget_conf = SettingsDict(
    font=FONT,
    fontsize=FONT_SIZE,
    margin=MARGIN,
)

extension_defaults = widget_conf.copy()

# Status bar
screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    border_width=1,
                    disable_drag=True,
                    highlight_method="line",
                    **widget_conf.override(
                        fontsize=ICON_SIZE
                    )
                ),
                widget.WindowName(**widget_conf),
                widget.Systray(**widget_conf),
                widget.Clock(
                    **widget_conf,
                    padding=10
                ),
            ],

            margin=MARGIN,
            size=26,
            border_width=2,
            background=NORMAL_COLOR,
            border_color=[
                FOCUS_COLOR,
                FOCUS_COLOR,
                FOCUS_COLOR,
                FOCUS_COLOR
            ]
        ),
        left=bar.Gap(MARGIN),
        right=bar.Gap(MARGIN),
        bottom=bar.Gap(MARGIN)
    ),
]


# focus window on mouse hover
follow_mouse_focus = True

dgroups_app_rules = []

bring_front_click = True

cursor_warp = False

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

