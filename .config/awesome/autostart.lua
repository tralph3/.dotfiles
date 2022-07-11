local awful = require("awful")

local search_paths = {
    '/etc/xdg/autostart',
    string.format('%s/.config/autostart', os.getenv('HOME')),
    string.format('%s/.config/awesome/autostart', os.getenv('HOME')),
}

awful.spawn(string.format('dex -as %s', table.concat(search_paths, ':')))
