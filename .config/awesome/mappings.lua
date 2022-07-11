local awful = require('awful')
local gears = require('gears')
local commands = require('commands')
local modkey = 'Mod4'

local globalkeys = gears.table.join(
    awful.key({ modkey }, 'Tab', awful.tag.history.restore),

    awful.key({ modkey }, 'h', function ()
        awful.client.focus.bydirection('left')
    end),
    awful.key({ modkey }, 'j', function ()
        awful.client.focus.bydirection('down')
    end),
    awful.key({ modkey }, 'k', function ()
        awful.client.focus.bydirection('up')
    end),
    awful.key({ modkey }, 'l', function ()
        awful.client.focus.bydirection('right')
    end),

    awful.key({ modkey, 'Shift' }, 'h', function ()
        awful.client.swap.bydirection('left')
    end),
    awful.key({ modkey, 'Shift' }, 'j', function ()
        awful.client.swap.bydirection('down')
    end),
    awful.key({ modkey, 'Shift' }, 'k', function ()
        awful.client.swap.bydirection('up')
    end),
    awful.key({ modkey, 'Shift' }, 'l', function ()
        awful.client.swap.bydirection('right')
    end),

    awful.key({ modkey }, 'Return', function()
        awful.spawn.with_shell(commands.terminal)
    end),

    awful.key({ modkey }, 'r', function()
            awful.spawn.with_shell(commands.run_prompt)
    end),

    awful.key({ modkey, 'Control' }, 'r', awesome.restart),
    awful.key({ modkey, 'Control' }, 'q', awesome.quit),

    awful.key({ modkey, 'Control' }, 'l', function()
        awful.tag.incmwfact(0.05)
    end),
    awful.key({ modkey, 'Control' }, 'h', function()
        awful.tag.incmwfact(-0.05)
    end),

    awful.key({}, 'XF86AudioRaiseVolume', function()
        awful.spawn.with_shell(commands.raise_volume)
    end),
    awful.key({}, 'XF86AudioLowerVolume', function()
        awful.spawn.with_shell(commands.lower_volume)
    end),
    awful.key({}, 'XF86AudioMute', function()
        awful.spawn.with_shell(commands.toggle_mute_audio)
    end),
    awful.key({}, 'XF86AudioMicMute', function()
        awful.spawn.with_shell(commands.toggle_mute_mic)
    end),

    awful.key({}, 'XF86MonBrightnessUp', function()
        awful.spawn.with_shell(commands.brightness_up)
    end),
    awful.key({}, 'XF86MonBrightnessDown', function()
        awful.spawn.with_shell(commands.brightness_down)
    end),

    awful.key({ modkey }, 'i', function()
        awful.spawn.with_shell(commands.browser)
    end),
    awful.key({ modkey, 'Shift' }, 'i', function()
        awful.spawn.with_shell(commands.browser_private)
    end),

    awful.key({}, 'Print', function()
        awful.spawn.with_shell(commands.screenshot)
    end),

    awful.key({ modkey, 'Control', 'Shift' }, 'q', function()
        awful.spawn.with_shell(commands.poweroff)
    end),
    awful.key({ modkey, 'Control', 'Shift' }, 'r', function()
        awful.spawn.with_shell(commands.reboot)
    end)
)

for i = 1, 9 do
    globalkeys = gears.table.join(
        globalkeys,

        awful.key({ modkey }, '#'..i + 9, function()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
                tag:view_only()
            end
        end),

        awful.key({ modkey, 'Shift' }, '#'..i + 9, function()
            if client.focus then
                local tag = client.focus.screen.tags[i]
                if tag then
                    client.focus:move_to_tag(tag)
                end
            end
        end)
    )
end

local globalbuttons = {}

local clientkeys = gears.table.join(
    awful.key({ modkey }, 'f', function(c)
        c.fullscreen = not c.fullscreen
        c:raise()
    end),

    awful.key({ modkey }, 'w', function(c)
        c:kill()
    end),

    awful.key({ modkey }, 'space', awful.client.floating.toggle),

    awful.key({ modkey }, 't', function(c)
        c.ontop = not c.ontop
    end)
)

local clientbuttons = gears.table.join(
    awful.button({}, 1, function(c)
        c:emit_signal('request::activate', 'mouse_click', { raise = true })
    end),
    awful.button({ modkey }, 1, function(c)
        c:emit_signal('request::activate', 'mouse_click', { raise = true })
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function(c)
        c:emit_signal('request::activate', 'mouse_click', { raise = true })
        awful.mouse.client.resize(c)
    end)
)

return {
    globalkeys = globalkeys,
    globalbuttons = globalbuttons,
    clientkeys = clientkeys,
    clientbuttons = clientbuttons,
}

