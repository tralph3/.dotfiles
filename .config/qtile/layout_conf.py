from libqtile.config import Match
from libqtile import layout
from utils import SettingsDict
from settings import layout_default

###########
# LAYOUTS #
###########
layouts = [
    layout.Columns(**layout_default),
    layout.Floating(**layout_default),
]

# Floating window config (different from the layout)
floating_layout = layout.Floating(
    **layout_default.extend(
        float_rules=[
            *layout.Floating.default_float_rules,
            Match(wm_class="confirmreset"),  # gitk
            Match(wm_class="makebranch"),  # gitk
            Match(wm_class="maketag"),  # gitk
            Match(wm_class="ssh-askpass"),  # ssh-askpass
            Match(wm_class="flameshot"),  # Flameshot upload window
            Match(wm_class="pavucontrol"),  # Pulse Audio Volume Control
            Match(wm_class="helvum"),  # Volume control GUI for Pipewire
            Match(title="branchdialog"),  # gitk
            Match(title="pinentry"),  # GPG key password entry
        ],
    ),
)
