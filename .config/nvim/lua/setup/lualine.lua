require'lualine'.setup{
    options = {
        icons_enabled = true,
        theme = 'auto',
        component_separators = { left = '', right = ''},
        section_separators = { left = '', right = ''},
        disabled_filetypes = {},
        always_divide_middle = true
    },

    sections = {
        lualine_a = {'mode'},
        lualine_b = {'branch', 'diff',
        {'diagnostics', sources={'nvim_diagnostic'}}},
        lualine_c = {{'filename', path=1}},
        lualine_x = {'encoding', 'fileformat', 'filetype'},
        lualine_y = {'progress'},
        lualine_z = {'location'}
    },

    tabline = {
        lualine_a = {'buffers'},
        lualine_z = {'tabs'}
    },
}
