vim.o.completeopt = "menu,noinsert,preview,longest,menuone,noselect"

local cmp = require('cmp')
local cmp_buffer = require('cmp_buffer')

local lsp_symbols = {
    Text = '  ',
    Method = '  ',
    Function = '  ',
    Constructor = '  ',
    Field = '  ',
    Variable = '  ',
    Class = '  ',
    Interface = '  ',
    Module = '  ',
    Property = '  ',
    Unit = '  ',
    Value = '  ',
    Enum = '  ',
    Keyword = '  ',
    Snippet = '  ',
    Color = '  ',
    File = '  ',
    Reference = '  ',
    Folder = '  ',
    EnumMember = '  ',
    Constant = '  ',
    Struct = '  ',
    Event = '  ',
    Operator = '  ',
    TypeParameter = '  ',
}

cmp.setup {
    snippet = {
        expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)
        end,
    },

    enabled = true,
    debug = false,
    throttle_time = 80,
    source_timeout = 200,
    incomplete_delay = 400,
    max_abbr_width = 100,
    max_kind_width = 100,
    max_menu_width = 100,
    documentation = true,

    completion = {
        keyword_length = 1,
    },

    mapping = {
        ["<C-j>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Select },
        ["<C-k>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Select },
        ["<C-d>"] = cmp.mapping.scroll_docs(-4),
        ["<C-u>"] = cmp.mapping.scroll_docs(4),
        ['<Tab>'] = cmp.mapping.confirm({ select = true }), --Automatic autocomplete
        ['<CR>'] = cmp.mapping.confirm({ select = false }), --Explicit autocomplete
    },

    sources = {
        { name = 'nvim_lsp' },
        { name = 'buffer',
            options = {
                get_bufnrs = function()
                    return vim.api.nvim_list_bufs()
                end,
            },
        },
        { name = 'path' },
    },

    sorting = {
        comparators = {
            function(...) return cmp_buffer:compare_locality(...) end,
        },
    },

    formatting = {
        fields = { "kind", "abbr", "menu"},
        format = function(entry, item)
            kind_name = item.kind
            item.kind = lsp_symbols[item.kind] or ""
            item.menu = ({
                buffer = "[B]",
                nvim_lsp = "[L]",
                path = "[P]"
            })[entry.source.name].." "..kind_name
            return item
        end,
    },

    experimental = {
        ghost_text = true,
    },
}
