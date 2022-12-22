from colors import USER_HOME, load_colorscheme
from utils import SettingsDict

colors = load_colorscheme()
FG_LIGHT = colors["FOREGROUND_1"]
FG_DARK = ""

BG_DARK = colors["BACKGROUND_1"]
BG_LIGHT = colors["BACKGROUND_2"]

WINDOW_FOCUS = colors["HIGHLIGHT_1"]
WINDOW_UNFOCUS = colors["HIGHLIGHT_2"]

BAR_BORDER = colors["HIGHLIGHT_2"]

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

mod = "mod4"
volume_step = 5
brightness_step = 5

commands = dict(
    terminal="alacritty",
    emacs="emacsclient -c",
    change_color=f"sh {USER_HOME}/.dotfiles/scripts/change_colorscheme.sh",
    raise_volume=f"sh {USER_HOME}/.config/qtile/scripts/audio.sh raise {volume_step}",
    lower_volume=f"sh {USER_HOME}/.config/qtile/scripts/audio.sh lower {volume_step}",
    play_pause_audio="playerctl play-pause",
    stop_audio="playerctl stop",
    skip_audio="playerctl next",
    prev_audio="playerctl previous",
    kill_window="xkill",
    switch_audio_sink=f"sh {USER_HOME}/.config/qtile/scripts/audio.sh switch_sink",
    toggle_mute=f"sh {USER_HOME}/.config/qtile/scripts/audio.sh mute",
    toggle_mute_mic="amixer set Capture toggle",
    rofi="rofi -show drun",
    firefox="firefox",
    firefox_private="firefox --private-window",
    flameshot="flameshot gui",
    poweroff="poweroff",
    reboot="reboot",
    brightness_up=f"sh {USER_HOME}/.config/qtile/scripts/brightness.sh raise {brightness_step}",
    brightness_down=f"sh {USER_HOME}/.config/qtile/scripts/brightness.sh lower {brightness_step}",
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
