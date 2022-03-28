import os
import subprocess

import libqtile
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

# Autostart
@hook.subscribe.startup_once
def autostart():
    script_path = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.run([script_path])

