local beautiful = require('beautiful')
local keys = require('mappings')
local awful = require('awful')

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
