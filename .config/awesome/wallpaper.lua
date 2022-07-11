local gears = require('gears')
local awful = require('awful')
local settings = require('settings')

local function set_random_wallpaper(wallpapers)
    math.randomseed(os.time())
    local wallpaper = wallpapers[math.random(#wallpapers)]
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
                set_random_wallpaper(images)
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
