local awful = require('awful')
local beautiful = require('beautiful')
local set_wallpaper = require('wallpaper').set_wallpaper

client.connect_signal('manage', function (c)
    if not awesome.startup then
        awful.client.setslave(c)
    end
    if awesome.startup
        and not c.size_hints.user_position
        and not c.size_hints.program_position
    then
        awful.placement.no_offscreen(c)
    end
end)

client.connect_signal('request::manage', function(c)
    c.maximized = false
end)
client.connect_signal('mouse::enter', function(c)
    c:emit_signal('request::activate', 'mouse_enter', {raise = false})
end)
client.connect_signal('focus', function(c) c.border_color = beautiful.border_focus end)
client.connect_signal('unfocus', function(c) c.border_color = beautiful.border_normal end)
screen.connect_signal("property::geometry", function()
    set_wallpaper(_G.current_wallpaper)
end)
