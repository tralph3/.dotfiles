vim.o.completeopt = "menuone,noselect"

local cmp = require'cmp'

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
        ["<C-j>"] = cmp.mapping.select_next_item(),
        ["<C-k>"] = cmp.mapping.select_prev_item(),
        ['<Tab>'] = cmp.mapping.confirm({ select = true }), --Automatic autocomplete
        ['<CR>'] = cmp.mapping.confirm({ select = false }), --Explicit autocomplete
    },

    sources = {
        { name = 'nvim_lsp' },  --LSP autocompletions
        { name = 'buffer' },    --Words in the current buffer
        { name = 'path' }       --File system
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
}
