from libqtile import bar, widget
from libqtile.config import Screen
from libqtile.lazy import lazy
from settings import (
    BACKGROUND1,
    BACKGROUND2,
    FOCUS_COLOR,
    FOCUS_FOREGROUND,
    FONT_SIZE,
    FOREGROUND,
    ICON_SIZE,
    MARGIN,
    commands,
    separator_default,
    widget_default,
)


###########
# WIDGETS #
###########
def create_separator(side):
    if side == "left":
        symbol = ""
    elif side == "right":
        symbol = ""

    separator = widget.TextBox(
        **separator_default.extend(
            foreground=FOREGROUND,
            background=BACKGROUND2,
            text=symbol
        ),
    ),
    return separator[0]


# Status bar
screens = [
    Screen(
        top=bar.Bar([
                # Arch logo
                widget.Image(
                    **widget_default.extend(
                        mouse_callbacks={
                            "Button1": lazy.spawn(commands["rofi"])
                        },
                        filename="~/.config/qtile/archlinux-icon.svg",
                    ),
                ),
                create_separator("left"),
                # Group Box
                widget.GroupBox(
                    disable_drag=True,
                    **widget_default.extend(
                        fontsize=ICON_SIZE,
                        margin=3,
                        inactive=BACKGROUND1,
                    ),
                ),
                create_separator("left"),
                # Layout Indicator
                widget.CurrentLayout(**widget_default),

                # Separators
                widget.TextBox(
                    **separator_default,
                    text=""
                ),
                widget.TextBox(margin=MARGIN, background=BACKGROUND1),

                widget.TaskList(
                    **widget_default.extend(
                        margin=0,
                        background=BACKGROUND1,
                        borderwidth=0,
                        max_title_width=200,
                        txt_floating="[F] ",
                        txt_maximized="[M] ",
                        txt_minimized="[m] ",
                        icon_size=FONT_SIZE,
                        padding_x=10,
                        padding_y=5,
                        mouse_callbacks={"Button2": lazy.window.kill()}
                    ),
                ),
                # Separators
                widget.TextBox(margin=MARGIN, background=BACKGROUND1),
                widget.TextBox(
                    **separator_default,
                    text=""
                ),

                # Systray
                widget.Systray(**widget_default),
                create_separator("right"),
                # Clock
                widget.Clock(
                    **widget_default.extend(
                        format="%H:%M",
                    ),
                ),
            ],

            size=26,
            margin=MARGIN,
            border_width=2,
            background=BACKGROUND2,
            border_color=FOCUS_COLOR,
        ),
        left=bar.Gap(MARGIN),
        right=bar.Gap(MARGIN),
        bottom=bar.Gap(MARGIN)
    ),
]
