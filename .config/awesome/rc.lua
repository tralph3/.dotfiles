package.loaded['naughty.dbus'] = {}

require('autostart')
require('awful.autofocus')
require('errors')
require('layouts')
require('bar')
require('signals')
require('rules')
local beautiful = require('beautiful')

local theme_path = string.format(
    '%s/.config/awesome/theme.lua', os.getenv('HOME'))

beautiful.init(theme_path)
