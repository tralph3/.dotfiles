local theme_assets = require('beautiful.theme_assets')
local dpi = require('beautiful.xresources').apply_dpi
local theme = {}

theme.font          = "UbuntuMono NerdFont Mono 12"

theme.bg_normal     = "#1E1E2E"
theme.bg_focus      = "#302D41"
theme.bg_urgent     = "#ff0000"
theme.bg_minimize   = "#444444"
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = "#D9E0EE"
theme.fg_focus      = "#ffffff"
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#ffffff"

theme.useless_gap   = dpi(3)
theme.border_width  = dpi(2)
theme.border_normal = "#302D41"
theme.border_focus  = "#89DCEB"
theme.border_marked = "#91231C"

local taglist_square_size = dpi(2)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.fg_normal
)

theme.icon_theme = 'Papirus-Dark'

return theme
