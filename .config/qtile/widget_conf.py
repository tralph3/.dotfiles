from libqtile import bar, widget
from libqtile.config import Screen
from libqtile.lazy import lazy
from settings import (
    BAR_BORDER,
    BG_DARK,
    BG_LIGHT,
    FG,
    FONT_SIZE,
    GROUP_ACTIVE,
    GROUP_INACTIVE,
    ICON_SIZE,
    MARGIN,
    TASK_SELECTED,
    commands,
    widget_default,
)


###########
# WIDGETS #
###########
def create_separator():
    separator = widget.TextBox(
        fontsize=30,
        foreground=FG,
        background=BG_DARK,
        margin=0,
        padding=0,
        text=""
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
                create_separator(),
                # Group Box
                widget.GroupBox(
                    **widget_default.extend(
                        disable_drag=True,
                        fontsize=ICON_SIZE,
                        margin=3,
                        active=GROUP_ACTIVE,
                        inactive=GROUP_INACTIVE,
                    ),
                ),
                create_separator(),
                # Layout Indicator
                widget.CurrentLayout(**widget_default),

                # Separators
                widget.TextBox(
                    background=BG_LIGHT,
                    fontsize=30,
                    foreground=BG_DARK,
                    margin=0,
                    padding=0,
                    text="",
                ),
                widget.TextBox(margin=MARGIN, background=BG_LIGHT),

                widget.TaskList(
                    **widget_default.extend(
                        background=BG_LIGHT,
                        border=TASK_SELECTED,
                        borderwidth=0,
                        icon_size=FONT_SIZE,
                        margin=0,
                        max_title_width=200,
                        mouse_callbacks={"Button2": lazy.window.kill()},
                        padding_x=10,
                        padding_y=5,
                        txt_floating="[F] ",
                        txt_maximized="[M] ",
                        txt_minimized="[m] ",
                    ),
                ),
                # Separators
                widget.TextBox(margin=MARGIN, background=BG_LIGHT),
                widget.TextBox(
                    background=BG_DARK,
                    fontsize=30,
                    foreground=BG_LIGHT,
                    margin=0,
                    padding=0,
                    text="",
                ),

                # Systray
                widget.Systray(**widget_default),
                create_separator(),
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
            background=BG_DARK,
            border_color=BAR_BORDER,
        ),
        left=bar.Gap(MARGIN),
        right=bar.Gap(MARGIN),
        bottom=bar.Gap(MARGIN)
    ),
]
