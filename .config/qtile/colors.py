import json
import os

HOME = os.path.expanduser('~')


def load_colorscheme():
    colors = {}
    with open(f"{HOME}/.config/colorschemes/current_colorscheme/colors.json") as f:
        colors = json.load(f)
    return colors
