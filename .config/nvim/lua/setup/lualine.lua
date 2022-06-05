local icons = require('config.globals').icons

require('lualine').setup({
    options = {
        icons_enabled = true,
        theme = 'auto',
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
        disabled_filetypes = {},
        always_divide_middle = false,
        globalstatus = true,
    },

    sections = {
        lualine_a = { 'mode' },
        lualine_b = {
            'branch',
            'diff',
            { 'diagnostics',
                sources = { 'nvim_diagnostic', 'nvim_lsp' },
                symbols = {
                    error = icons.Error..' ',
                    warn = icons.Warning..' ',
                    info = icons.Info..' ',
                    hint = icons.Hint..' ',
                },
            },
        },
        lualine_c = {
            { 'filename',
                path = 3,
                symbols = {
                    modified = ' '..icons.Modified,
                    readonly = ' '..icons.Readonly,
                },
            },
        },
        lualine_x = {
            'encoding',
            'fileformat',
            'filetype',
        },
        lualine_y = { 'progress' },
        lualine_z = { 'location' },
    },

    tabline = {
        lualine_a = {
            { 'buffers',
                max_length = vim.o.columns,
                filetype_names = {
                    ['neo-tree'] = string.format('%s Files', icons.Folder),
                },
                symbols = {
                    modified = ' '..icons.Modified,
                    alternate_file = '',
                },
            },
        },
    },
})
