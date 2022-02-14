import os
import subprocess

from libqtile import hook
from libqtile.dgroups import simple_key_binder

from settings import mod
import group_conf
import key_conf
import layout_conf
import widget_conf

# init groups
groups = group_conf.init()
# init keybindings
keys, mouse = key_conf.init()
# init layouts
layouts, floating_layout = layout_conf.init()
# init widgets
screens = widget_conf.init()

# focus window on mouse hover
follow_mouse_focus = True

# Binds "mod + X" to switch between grops
dgroups_key_binder = simple_key_binder(mod)
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
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.run([home])

