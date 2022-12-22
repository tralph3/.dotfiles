import os

USER_HOME = os.path.expanduser('~')


def load_colorscheme():
    colors = {}
    with open(f"{USER_HOME}/.config/colorschemes/current_colorscheme") as f:
        colorscheme = f.read()[:-1]

    with open(f"{USER_HOME}/.config/colorschemes/{colorscheme}.scheme") as f:
        for line in f.readlines():
            if line == '\n':
                continue
            color, value = line.split("=")
            colors[color] = value[:-1]

    return colors
