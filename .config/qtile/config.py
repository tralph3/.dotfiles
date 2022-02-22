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
import widget_conf
screens = widget_conf.init()

# focus window on mouse hover
follow_mouse_focus = True

dgroups_app_rules = []

bring_front_click = True

cursor_warp = False

auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

auto_minimize = True

wmname = "LG3D"

# Autostart
@hook.subscribe.startup_once
def autostart():
    script_path = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.run([script_path])

