local awful = require("awful")

local autostart_programs = {
    '/usr/bin/setxkbmap -layout latam',
    '/usr/bin/flameshot',
    '/usr/bin/numlockx on',
    '/usr/bin/dunst',
    '/usr/bin/dex -a',
    '/usr/bin/picom --experimental-backends --unredir-if-possible'
}

for _, app in pairs(autostart_programs) do
    awful.util.spawn(app)
end
