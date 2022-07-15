local M = {}

local gears = require('gears')
local awful = require('awful')
local settings = require('settings')

math.randomseed(os.time())

M.set_wallpaper = function(wallpaper)
    if not wallpaper then
        return
    end

    _G.current_wallpaper = wallpaper
    for s in screen do
        gears.wallpaper.maximized(wallpaper, s)
    end
end

local function set_random_wallpaper_from_dir(dir)
    local images = {}
    local command = string.format('find "%s" -iname "*.jpg" -type f', dir)
    awful.spawn.with_line_callback(command, {
        stdout = function(line)
            table.insert(images, line)
        end,
        output_done = function()
            if #images > 0 then
                local wallpaper = images[math.random(#images)]
                M.set_wallpaper(wallpaper)
            end
        end
    })
end


gears.timer({
    timeout = settings.background_switch_interval_in_minutes * 60,
    call_now = true,
    autostart = true,
    callback = function()
        set_random_wallpaper_from_dir(settings.backgrounds_directory)
    end
})

return M
