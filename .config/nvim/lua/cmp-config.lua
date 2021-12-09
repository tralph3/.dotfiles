vim.o.completeopt = "menuone,noselect"

local cmp = require'cmp'
cmp.setup {
    snippet = {
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
      end,
    },

    enabled = true,
    autocomplete = true,
    debug = false,
    min_length = 0,
    preselect = 'enable',
    throttle_time = 80,
    source_timeout = 200,
    incomplete_delay = 400,
    max_abbr_width = 100,
    max_kind_width = 100,
    max_menu_width = 100,
    documentation = true,

    mapping = {
        ['<Tab>'] = cmp.mapping.confirm({ select = true }),
    },

    sources = {
        { name = 'nvim_lsp' },
        { name = 'buffer' },
        { name = 'path' }
    }
}
