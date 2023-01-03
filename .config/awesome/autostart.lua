local awful = require("awful")

DOTFILES_DIR = os.getenv('DOTFILES_DIR')

awful.spawn(DOTFILES_DIR..'/scripts/autostart_all.sh')
