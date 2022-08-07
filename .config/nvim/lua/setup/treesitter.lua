local tree_sitter_parser_path = vim.fn.expand("~/.local/share/treesitter-parsers")
vim.opt.runtimepath:append(tree_sitter_parser_path)
require('nvim-treesitter.configs').setup({
    ensure_installed = {
        'bash',
        'c',
        'cmake',
        'comment',
        'cpp',
        'css',
        'dockerfile',
        'gdscript',
        'haskell',
        'html',
        'javascript',
        'json',
        'latex',
        'lua',
        'make',
        'markdown',
        'python',
        'regex',
        'rust',
        'vim',
        'yaml',
    },
    parser_install_dir = tree_sitter_parser_path,
    sync_install = true,
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = true,
    },
    indent = {
        enable = false,
    },
    textobjects = {
        select = {
            enable = true,
            lookahead = true,
            keymaps = {
                ['af'] = '@function.outer',
                ['if'] = '@function.inner',
                ['ac'] = '@class.outer',
                ['ic'] = '@class.inner',
                ['ap'] = '@parameter.outer',
                ['ip'] = '@parameter.inner',
            },
        },
        swap = {
            enable = true,
            swap_next = {
                ['<C-l>'] = '@parameter.inner',
            },
            swap_previous = {
                ['<C-h>'] = '@parameter.inner',
            }
        },
    },
})
