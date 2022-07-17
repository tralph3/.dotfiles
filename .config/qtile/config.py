import os
import subprocess

from libqtile import hook
from libqtile.widget import base

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


search_paths = [
    '/etc/xdg/autostart',
    os.path.expanduser('~/.config/autostart'),
    os.path.expanduser('~/.config/qtile/autostart'),
]


@hook.subscribe.startup_once
def autostart():
    autostart_paths = ':'.join(search_paths)
    subprocess.run(['/usr/bin/dex', '-as', autostart_paths])


@hook.subscribe.startup_once
def setup_wallpaper_timer():
    wallpaper.Timer(
        WALLPAPER_TIMEOUT_MINUTES * 60, wallpaper.set_random_wallpaper)
