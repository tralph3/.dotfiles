from libqtile import bar, widget
from libqtile.config import Screen
from libqtile.lazy import lazy
from settings import (
    BACKGROUND1,
    BACKGROUND2,
    ACCENT1,
    ACCENT2,
    DOTFILES_DIR,
    FOREGROUND1,
    FONT,
    FONT_SIZE,
    ICON_SIZE,
    MARGIN,
    commands,
    widget_default,
)


###########
# WIDGETS #
###########
def create_separator():
    separator = widget.TextBox(
        fontsize=30,
        font=FONT,
        foreground=FOREGROUND1,
        background=BACKGROUND1,
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
                        filename=f"{DOTFILES_DIR}/assets/archlinux-icon.svg",
                    ),
                ),
                create_separator(),
                # Group Box
                widget.GroupBox(
                    **widget_default.extend(
                        disable_drag=True,
                        fontsize=ICON_SIZE,
                        margin=3,
                        active=FOREGROUND1,
                        inactive=BACKGROUND2,
                    ),
                ),
                # Separators
                widget.TextBox(
                    background=BACKGROUND2,
                    fontsize=30,
                    font=FONT,
                    foreground=BACKGROUND1,
                    margin=0,
                    padding=0,
                    text="",
                ),
                widget.TextBox(margin=MARGIN, background=BACKGROUND2),

                widget.TaskList(
                    **widget_default.extend(
                        background=BACKGROUND2,
                        border=ACCENT2,
                        borderwidth=0,
                        icon_size=FONT_SIZE,
                        margin=0,
                        max_title_width=300,
                        mouse_callbacks={"Button2": lazy.window.kill()},
                        padding_x=10,
                        padding_y=5,
                        txt_floating="[F] ",
                        txt_maximized="[M] ",
                        txt_minimized="[m] ",
                    ),
                ),
                # Separators
                widget.TextBox(margin=MARGIN, background=BACKGROUND2),
                widget.TextBox(
                    background=BACKGROUND1,
                    fontsize=30,
                    font=FONT,
                    foreground=BACKGROUND2,
                    margin=0,
                    padding=0,
                    text="",
                ),

                # Systray
                widget.Systray(**widget_default),
                create_separator(),
                # Clock svg
                widget.Image(
                    **widget_default.extend(
                        filename=f"{DOTFILES_DIR}/assets/clock.svg",
                    ),
                ),
                # Clock
                widget.Clock(
                    **widget_default.extend(
                        format="%H:%M\n%y/%m/%d",
                        fontsize=12,
                    ),
                ),
            ],

            size=26,
            margin=MARGIN,
            border_width=2,
            background=BACKGROUND1,
            border_color=ACCENT2,
        ),
        left=bar.Gap(MARGIN),
        right=bar.Gap(MARGIN),
        bottom=bar.Gap(MARGIN)
    ),
]
