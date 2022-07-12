package.loaded['naughty.dbus'] = {}

require('autostart')
require('wallpaper')
local keys = require('mappings')
require('awful.autofocus')

local gears = require('gears')
local awful = require('awful')
local wibox = require('wibox')
local beautiful = require('beautiful')
local naughty = require('naughty')
local hotkeys_popup = require('awful.hotkeys_popup')
local commands = require('commands')

if awesome.startup_errors then
    naughty.notify({
        title = 'There were errors during startup.',
        preset = naughty.config.presets.critical,
        text = awesome.startup_errors,
    })
end

local in_error = false
awesome.connect_signal('debug::error', function(err)
    if in_error then
        return
    end
    in_error = true
    naughty.notify({
        title = 'An error occurred.',
        text = tostring(err),
        preset = naughty.config.presets.critical,
    })
    in_error = false
end)

local theme_path = string.format(
    '%s/.config/awesome/theme.lua', os.getenv('HOME'))

beautiful.init(theme_path)

local modkey = 'Mod4'

awful.layout.layouts = {
    awful.layout.suit.tile,
}

mytextclock = wibox.widget.textclock()

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
    awful.button({ }, 1, function(t) t:view_only() end),
    awful.button({ modkey }, 1, function(t)
              if client.focus then
                  client.focus:move_to_tag(t)
              end
          end),
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, function(t)
              if client.focus then
                  client.focus:toggle_tag(t)
              end
          end),
    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  c:emit_signal(
                                                      'request::activate',
                                                      'tasklist',
                                                      {raise = true}
                                                  )
                                              end
                                          end),
                     awful.button({ }, 3, function()
                                              awful.menu.client_list({ theme = { width = 250 } })
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == 'function' then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal('property::geometry', set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    local tag_titles = {'', '', '', '', '', '', '', '', ''}
    awful.tag(tag_titles, s, awful.layout.layouts[1])

    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons
    }

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }

    -- Create the wibox
    s.mywibox = awful.wibar({ position = 'top', screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            s.mytaglist,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            wibox.widget.systray(),
            mytextclock,
        },
    }
end)

root.keys(keys.globalkeys)

awful.rules.rules = {
    {
        rule = { },
        properties = {
            border_color = beautiful.border_normal,
            border_width = beautiful.border_width,
            buttons = keys.clientbuttons,
            focus = awful.client.focus.filter,
            keys = keys.clientkeys,
            placement = awful.placement.no_overlap+awful.placement.no_offscreen,
            raise = true,
            screen = awful.screen.preferred,
            size_hints_honor = false,
            titlebars_enabled = false,
        },
    },

    {
        rule_any = {
            instance = {
                'DTA',
                'branchdialog',
                'copyq',
                'pinentry',
            },
            class = {
                'Arandr',
                'Blueman-manager',
                'Gpick',
                'Kruler',
                'MessageWin',
                'Sxiv',
                'Tor Browser',
                'Wpa_gui',
                'confirmreset',
                'flameshot',
                'helvum',
                'makebranch',
                'maketag',
                'mpv',
                'pavucontrol',
                'ssh-askpass',
                'veromix',
                'xtightvncviewer',
            },

            name = {
                'Event Tester',
            },
            role = {
                'AlarmWindow',
                'ConfigManager',
                'pop-up',
            },
        },
        properties = {
            floating = true
        }
    },

    {
        rule_any = {
            class = {
                'firefox',
            },
        },
        properties = { screen = 1, tag = screen[1].tags[2] },
    },

    {
        rule_any = {
            class = {
                'thunar',
                'engrampa',
            },
        },
        properties = { screen = 1, tag = screen[1].tags[4] },
    },

    {
        rule_any = {
            class = {
                'Steam',
                'Lutris',
                'gamescope',
            },
        },
        properties = { screen = 1, tag = screen[1].tags[5] },
    },

    {
        rule_any = {
            class = {
                'discord',
            },
        },
        properties = { screen = 1, tag = screen[1].tags[6] },
    },

    {
        rule_any = {
            class = {
                'Spotify',
                'Quodlibet',
            },
        },
        properties = { screen = 1, tag = screen[1].tags[7] },
    },
}

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
