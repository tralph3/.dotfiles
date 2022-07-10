local gears = require('gears')
local awful = require('awful')

local function list_images_in_dir(dir)
    local images = {}
    local p = io.popen(
        string.format('find "%s" -iname "*.jpg" -type f', dir)
    )
    if not p then
       return {}
    end
    for image in p:lines() do
        table.insert(images, image)
    end
    p:close()
    return images
end

local function set_random_wallpaper(wallpapers)
    math.randomseed(os.time())
    local wallpaper = wallpapers[math.random(#wallpapers)]
    for screen in awful.screen do
        gears.wallpaper.maximized(wallpaper, screen)
    end
end

gears.timer({
    timeout = 600,
    call_now = true,
    autostart = true,
    callback = function()
        local backgrounds_path = string.format(
            '%s/.local/share/backgrounds', os.getenv('HOME'))
        local images = list_images_in_dir(backgrounds_path)
        set_random_wallpaper(images)
    end
})
