import os
import subprocess

from libqtile import hook

# init groups
from group_conf import groups

# init keybindings
from key_conf import keys, mouse

# init layouts
from layout_conf import layouts, floating_layout

# init widgets
from widget_conf import screens

# general settings
from options import *

import wallpaper
from settings import WALLPAPER_TIMEOUT_MINUTES

DOTFILES_DIR = os.getenv("DOTFILES_DIR")


@hook.subscribe.startup_once
def autostart():
    subprocess.run([f"{DOTFILES_DIR}/scripts/autostart_all.sh"])


@hook.subscribe.startup_once
def setup_wallpaper_timer():
    wallpaper.Timer(WALLPAPER_TIMEOUT_MINUTES * 60, wallpaper.set_random_wallpaper)
