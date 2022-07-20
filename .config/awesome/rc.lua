package.loaded['naughty.dbus'] = {}

require('autostart')
require('awful.autofocus')
require('rules')
require('errors')
require('layouts')
require('bar')
require('signals')
local beautiful = require('beautiful')

local theme_path = string.format(
    '%s/.config/awesome/theme.lua', os.getenv('HOME'))

beautiful.init(theme_path)
