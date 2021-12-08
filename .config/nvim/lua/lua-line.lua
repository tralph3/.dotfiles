require'lualine'.setup{
    options = {
        icons_enabled = true,
        theme = 'codedark',
        component_separators = { left = '', right = ''},
        section_separators = { left = '', right = ''},
        disabled_filetypes = {},
        always_divide_middle = true,
    },

    sections = {
        lualine_a = {'mode'},
        lualine_b = {'branch', 'diff',
        {'diagnostics', sources={'nvim_lsp'}}},
        lualine_c = {'filename'},
        lualine_x = {'encoding', 'fileformat', 'filetype'},
        lualine_y = {'progress'},
        lualine_z = {'location'}
    },

    tabline = {
        lualine_a = {'buffers'},
        lualine_b = {'branch'},
        lualine_c = {'filename'},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {'tabs'}
    }
}
