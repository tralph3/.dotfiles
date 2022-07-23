local awful = require('awful')
local wibox = require('wibox')
local gears = require('gears')
local dpi = require('beautiful.xresources').apply_dpi
local taglist_buttons = require('mappings').taglist_buttons
local tasklist_buttons = require('mappings').tasklist_buttons
local text_clock = wibox.widget.textclock()

awful.screen.connect_for_each_screen(function(s)
    local tag_titles = { '', '', '', '', '', '', '', '', '' }
    local shape = gears.shape.rectangle
    awful.tag(tag_titles, s, awful.layout.layouts[1])

    s.taglist = awful.widget.taglist({
        screen = s,
        style = {
            shape = shape,
        },
        layout = {
            spacing = 5,
            spacing_widget = {
                shape = shape,
                wibox.widget.separator,
            },
            layout = wibox.layout.fixed.horizontal,
        },
        filter = awful.widget.taglist.filter.all,
        buttons = taglist_buttons,
    })

    -- Create a tasklist widget
    s.tasklist = awful.widget.tasklist({
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons,
    })

    s.wibox = awful.wibar({
        position = 'top',
        screen = s,
        margins = dpi(4),
    })

    s.wibox:setup({
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            s.taglist,
        },
        s.tasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            wibox.widget.systray(),
            text_clock,
        },
    })
end)
