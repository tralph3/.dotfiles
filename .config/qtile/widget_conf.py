from libqtile import bar, widget
from libqtile.config import Screen
from libqtile.lazy import lazy
from settings import widget_default, separator_default, BACKGROUND1, BACKGROUND2, ICON_SIZE, commands, MARGIN, FONT_SIZE, FOCUS_COLOR

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
            foreground="#FFFFFF",
            background=BACKGROUND1,
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
                        mouse_callbacks={"Button1": lazy.spawn(commands["rofi"])},
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
                        inactive=BACKGROUND2,
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
                widget.TextBox(margin=MARGIN, background=BACKGROUND2),

                widget.TaskList(
                    **widget_default.extend(
                        margin=0,
                        background=BACKGROUND2,
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
                widget.TextBox(margin=MARGIN, background=BACKGROUND2),
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
            background=BACKGROUND1,
            border_color=FOCUS_COLOR,
        ),
        left=bar.Gap(MARGIN),
        right=bar.Gap(MARGIN),
        bottom=bar.Gap(MARGIN)
    ),
]
