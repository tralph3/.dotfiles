import os
from colors import load_colorscheme
from utils import SettingsDict

colors = load_colorscheme()
BACKGROUND1 = colors["BACKGROUND_1"]
BACKGROUND2 = colors["BACKGROUND_2"]
BACKGROUND3 = colors["BACKGROUND_3"]

FOREGROUND1 = colors["FOREGROUND_1"]
FOREGROUND2 = colors["FOREGROUND_2"]
FOREGROUND3 = colors["FOREGROUND_3"]

ACCENT1 = colors["ACCENT_1"]
ACCENT2 = colors["ACCENT_2"]
ACCENT3 = colors["ACCENT_3"]

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
    skip_audio="playerctl next",
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
    border_focus=ACCENT1,
    border_focus_stack=ACCENT1,
    border_normal=ACCENT2,
    border_normal_stack=ACCENT2,
    border_width=BORDER_WIDTH,
    margin=MARGIN,
    margin_on_single=MARGIN,
    border_on_single=True,
    fair=True,
)

widget_default = SettingsDict(
    background=BACKGROUND1,
    border=ACCENT1,
    font=FONT,
    fontsize=FONT_SIZE,
    foreground=FOREGROUND1,
    highlight_method="block",
    margin=MARGIN,
    other_current_screen_border=ACCENT2,
    other_screen_border=ACCENT2,
    rounded=False,
    this_current_screen_border=ACCENT2,
    this_screen_border=ACCENT2,
    urgent_alert_method="border",
)
