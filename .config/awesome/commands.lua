local settings = require('settings')

local commands = {
    terminal='alacritty',
    raise_volume='/usr/bin/pactl set-sink-volume 0 +'..settings.volume_step..'%',
    lower_volume='/usr/bin/pactl set-sink-volume 0 -'..settings.volume_step..'%',
    toggle_mute_audio='pactl set-sink-mute 0 toggle',
    toggle_mute_mic='amixer set Capture toggle',
    run_prompt = 'rofi -show drun',
    browser='firefox',
    browser_private='firefox --private-window',
    screenshot='flameshot gui',
    poweroff='poweroff',
    reboot='reboot',
    brightness_up='light -A '..settings.brightness_step,
    brightness_down='light -U '..settings.brightness_step,
}

return commands
