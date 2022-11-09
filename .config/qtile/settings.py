from utils import SettingsDict
import os

#############
# VARIABLES #
#############
FG_LIGHT = "#D9E0EE"
FG_DARK = ""

BG_DARK = "#161320"
BG_LIGHT = "#1E1E2E"

WINDOW_FOCUS = "#89DCEB"
WINDOW_UNFOCUS = "#302D41"

BAR_BORDER = "#302D41"

GROUP_ACTIVE = FG_LIGHT
GROUP_INACTIVE = BG_LIGHT
GROUP_SELECTED = BAR_BORDER

TASK_SELECTED = BAR_BORDER


MARGIN = 5
BORDER_WIDTH = 2

FONT = "UbuntuMono Nerd Font Mono"
ICON_SIZE = 25
FONT_SIZE = 15

WALLPAPERS_PATH = "/usr/share/backgrounds/"
WALLPAPER_TIMEOUT_MINUTES = 10

USER_HOME = os.path.expanduser("~")

mod = "mod4"
volume_step = 5
brightness_step = 5

# commands
commands = dict(
    terminal="alacritty",
    emacs="emacsclient -c",
    raise_volume=f"sh {USER_HOME}/.config/qtile/volume.sh raise {volume_step}",
    lower_volume=f"sh {USER_HOME}/.config/qtile/volume.sh lower {volume_step}",
    kill_window="xkill",
    toggle_mute="pactl set-sink-mute 0 toggle",
    toggle_mute_mic="amixer set Capture toggle",
    rofi="rofi -show drun",
    firefox="firefox",
    firefox_private="firefox --private-window",
    flameshot="flameshot gui",
    poweroff="poweroff",
    reboot="reboot",
    brightness_up=f"sh {USER_HOME}/.config/qtile/brightness.sh raise {brightness_step}",
    brightness_down=f"sh {USER_HOME}/.config/qtile/brightness.sh lower {brightness_step}",
)

layout_default = SettingsDict(
    border_focus=WINDOW_FOCUS,
    border_focus_stack=WINDOW_FOCUS,
    border_normal=WINDOW_UNFOCUS,
    border_normal_stack=WINDOW_UNFOCUS,
    border_width=BORDER_WIDTH,
    margin=MARGIN,
    margin_on_single=MARGIN,
    border_on_single=True,
    fair=True,
)

widget_default = SettingsDict(
    background=BG_DARK,
    border=BAR_BORDER,
    font=FONT,
    fontsize=FONT_SIZE,
    foreground=FG_LIGHT,
    highlight_method="block",
    margin=MARGIN,
    other_current_screen_border=GROUP_ACTIVE,
    other_screen_border=GROUP_ACTIVE,
    rounded=False,
    this_current_screen_border=GROUP_SELECTED,
    this_screen_border=GROUP_SELECTED,
    urgent_alert_method="border",
)
