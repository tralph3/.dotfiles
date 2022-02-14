from libqtile.config import Group, Click, Match

##########
# GROUPS #
##########
def init():
    groups = [
        # Terminal
        Group(""),

        # Web browser
        Group("", matches=[Match(wm_class=["firefox"])]),

        # Coding
        Group(""),

        # File browser
        Group("", matches=[
            Match(wm_class=["Thunar", "thunar"])
        ]),

        # Gaming stuff
        Group("", layout="floating", matches=[
            Match(wm_class=["Steam"]),
            Match(wm_class=["Lutris"]),
            Match(wm_class=["gamescope"]),
        ]),

        # Discord/Communication
        Group("", matches=[Match(wm_class=["discord"])]),

        # Music
        Group("", matches=[
            Match(wm_class=["spotify"]),
            Match(wm_class=["Quodlibet"])
        ]),

        # Image editing
        Group(""),

        # Anything else
        Group(""),
    ]

    return groups
