import os
import random
from libqtile import qtile
from typing import Callable
from settings import WALLPAPERS_DIR


class Timer():
    def __init__(self, timeout: int, callback: Callable) -> None:
        self.callback = callback
        self.timeout = timeout
        self.call()

    def call(self) -> None:
        self.callback()
        self.setup_timer()

    def setup_timer(self) -> None:
        self.timer = qtile.call_later(self.timeout, self.call)


def set_random_wallpaper() -> None:
    wallpapers = [
        os.path.join(WALLPAPERS_DIR, x) for x in os.listdir(WALLPAPERS_DIR) if x[-4:] == ".jpg"
    ]
    wallpaper = random.choice(wallpapers)
    set_wallpaper(wallpaper)


def set_wallpaper(file_path: str) -> None:
    for screen in qtile.screens:
        screen.cmd_set_wallpaper(file_path, 'fill')
