from utils import SettingsDict

#############
# VARIABLES #
#############
MARGIN = 5
BORDER_WIDTH = 2
FOCUS_COLOR = "#575268"
FOCUS_FOREGROUND = "#F5C2E7"
BACKGROUND1 = "#1E1D2F"
FOREGROUND = "#D9E0EE"
BACKGROUND2 = "#161320"
UNFOCUS_COLOR = "#575268"
BORDER_COLOR = "#FAE3B0"
FONT = "UbuntuMono Nerd Font Mono"
ICON_SIZE = 25
FONT_SIZE = 15
mod = "mod4"
volume_step = 5
brightness_step = 5

# commands
commands = dict(
    terminal="alacritty",
    raise_volume=f"/usr/bin/pactl set-sink-volume 0 +{volume_step}%",
    lower_volume=f"/usr/bin/pactl set-sink-volume 0 -{volume_step}%",
    toggle_mute="pactl set-sink-mute 0 toggle",
    toggle_mute_mic="amixer set Capture toggle",
    rofi="rofi -show drun",
    firefox="firefox",
    firefox_private="firefox --private-window",
    flameshot="flameshot gui",
    thunar="thunar",
    poweroff="poweroff",
    reboot="reboot",
    brightness_up=f"light -A {brightness_step}",
    brightness_down=f"light -U {brightness_step}",
)

layout_default = SettingsDict(
    border_focus=BORDER_COLOR,
    border_focus_stack=BORDER_COLOR,
    border_normal=UNFOCUS_COLOR,
    border_normal_stack=UNFOCUS_COLOR,
    border_width=BORDER_WIDTH,
    margin=MARGIN,
    margin_on_single=MARGIN,
    border_on_single=True,
    fair=True,
)

widget_default = SettingsDict(
    font=FONT,
    fontsize=FONT_SIZE,
    margin=MARGIN,
    background=BACKGROUND2,
    highlight_method="block",
    this_current_screen_border=FOCUS_COLOR,
    this_screen_border=FOCUS_COLOR,
    border=FOCUS_COLOR,
    other_current_screen_border=FOCUS_COLOR,
    other_screen_border=FOCUS_COLOR,
    urgent_alert_method="border",
    rounded=False,
)

separator_default = SettingsDict(
    fontsize=30,
    foreground=BACKGROUND2,
    background=BACKGROUND1,
    margin=0,
    padding=0,
)
