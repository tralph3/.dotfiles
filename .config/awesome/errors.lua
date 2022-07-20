local naughty = require('naughty')

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
