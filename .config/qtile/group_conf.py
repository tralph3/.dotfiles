from libqtile.config import Group, Match

##########
# GROUPS #
##########
groups = [
    # Terminal
    Group("1", label=""),

    # Web browser
    Group("2", label="", matches=[Match(wm_class=["firefox"])]),

    # Coding
    Group("3", label=""),

    # File manager
    Group("4", label="", matches=[
        Match(wm_class=["Thunar", "thunar"])
    ]),

    # Gaming stuff
    Group("5", label="", matches=[
        Match(wm_class=["Steam"]),
        Match(wm_class=["Lutris"]),
        Match(wm_class=["gamescope"]),
    ]),

    # Discord/Communication
    Group("6", label="", matches=[Match(wm_class=["discord"])]),

    # Music
    Group("7", label="", matches=[
        Match(wm_class=["Spotify", "spotify"]),
        Match(wm_class=["Quodlibet"])
    ]),

    # Image editing
    Group("8", label=""),

    # Anything else
    Group("9", label=""),
]
