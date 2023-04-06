import os
from colors import load_colorscheme
from utils import SettingsDict

colors = load_colorscheme()
BACKGROUND1 = colors["BACKGROUND_1"]
BACKGROUND2 = colors["BACKGROUND_2"]

FOREGROUND1 = colors["FOREGROUND_1"]
FOREGROUND2 = colors["FOREGROUND_2"]

ACCENT = colors["ACCENT"]

INACTIVE = colors["INACTIVE"]

HIGHLIGHT_BG = colors["HIGHLIGHT_BG"]
HIGHLIGHT_FG = colors["HIGHLIGHT_FG"]

MARGIN = 5
BORDER_WIDTH = 2

FONT = "UbuntuMono Nerd Font Mono"
ICON_SIZE = 25
FONT_SIZE = 15

WALLPAPER_TIMEOUT_MINUTES = 10
WALLPAPERS_DIR = os.getenv('WALLPAPERS_DIR')
DOTFILES_DIR = os.getenv('DOTFILES_DIR')

mod = "mod4"
volume_step = 5
brightness_step = 5

commands = dict(
    terminal="alacritty",
    emacs="emacsclient -c",
    change_color=f"sh {DOTFILES_DIR}/scripts/colorscheme/change_colorscheme.sh",
    raise_volume=f"sh {DOTFILES_DIR}/scripts/audio.sh raise {volume_step}",
    lower_volume=f"sh {DOTFILES_DIR}/scripts/audio.sh lower {volume_step}",
    calculator="speedcrunch",
    play_pause_audio="playerctl play-pause",
    stop_audio="playerctl stop",
    next_audio="playerctl next",
    prev_audio="playerctl previous",
    kill_window="xkill",
    decode_qr="sh -c \"flameshot gui --raw | zbarimg --raw - | xclip -selection clipboard\"",
    switch_audio_sink=f"sh {DOTFILES_DIR}/scripts/audio.sh switch_sink",
    toggle_mute=f"sh {DOTFILES_DIR}/scripts/audio.sh mute",
    toggle_mute_mic="amixer set Capture toggle",
    rofi="rofi -show drun",
    firefox="firefox",
    firefox_private="firefox --private-window",
    flameshot="flameshot gui",
    poweroff="poweroff",
    reboot="reboot",
    brightness_up=f"sh {DOTFILES_DIR}/scripts/brightness.sh raise {brightness_step}",
    brightness_down=f"sh {DOTFILES_DIR}/scripts/brightness.sh lower {brightness_step}",
)

layout_default = SettingsDict(
    border_focus=ACCENT,
    border_focus_stack=ACCENT,
    border_normal=INACTIVE,
    border_normal_stack=INACTIVE,
    border_width=BORDER_WIDTH,
    margin=MARGIN,
    margin_on_single=MARGIN,
    border_on_single=True,
    fair=True,
)

widget_default = SettingsDict(
    background=BACKGROUND1,
    border=ACCENT,
    font=FONT,
    fontsize=FONT_SIZE,
    foreground=FOREGROUND1,
    highlight_method="block",
    margin=MARGIN,
    other_current_screen_border=INACTIVE,
    other_screen_border=INACTIVE,
    rounded=False,
    this_current_screen_border=INACTIVE,
    this_screen_border=INACTIVE,
    urgent_alert_method="border",
)
