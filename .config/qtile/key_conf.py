from libqtile.config import Key, Drag, Click
from libqtile.lazy import lazy
from settings import commands, mod

############
# KEYBINDS #
############
def init():
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
        Key([], "XF86AudioRaiseVolume", lazy.spawn(commands["raise_volume"])),
        Key([], "XF86AudioLowerVolume", lazy.spawn(commands["lower_volume"])),
        Key([], "XF86AudioMute", lazy.spawn(commands["toggle_mute"])),
        Key([], "XF86AudioMicMute", lazy.spawn(commands["toggle_mute_mic"])),

        # Terminal
        Key([mod], "Return", lazy.spawn(commands["terminal"]), desc="Launch terminal"),

        # Rofi
        Key([mod], "r", lazy.spawn(commands["rofi"]), desc="Spawn a rofi Window"),

        # Firefox
        Key([mod], "i", lazy.spawn(commands["firefox"]), desc="Open Firefox"),
        Key([mod, "shift"], "i", lazy.spawn(commands["firefox_private"]), desc="Open Firefox in Incognito mode"),

        # Flameshot
        Key([], "Print", lazy.spawn(commands["flameshot"]), desc="Take screenshot"),

        # Thunar
        Key([mod], "f", lazy.spawn(commands["thunar"]), desc="File browser"),

        # Backlight
        Key([], "XF86MonBrightnessUp", lazy.spawn(commands["brightness_up"]), desc="Raise backlight brightness"),
        Key([], "XF86MonBrightnessDown", lazy.spawn(commands["brightness_down"]), desc="Lower backlight brightness"),

        # Qtile
        Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
        Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
        Key([mod, "control", "shift"], "q", lazy.spawn(commands["poweroff"]), desc="Shutdown computer"),
    ]

    # Drag floating layouts.
    mouse = [
        Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
        Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
        Click([mod], "Button2", lazy.window.bring_to_front()),
    ]

    return (keys, mouse)

