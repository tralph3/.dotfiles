vim.o.completeopt = 'menu,noinsert,preview,longest,menuone,noselect'

local cmp = require('cmp')

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

cmp.setup({
    snippet = {
        expand = function(args)
            vim.fn['UltiSnips#Anon'](args.body)
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

    window = {
        documentation = "native",
    },

    completion = {
        keyword_length = 1,
    },

    mapping = {
        ['<C-j>'] = cmp.mapping.select_next_item(
            { behavior = cmp.SelectBehavior.Select }),
        ['<C-k>'] = cmp.mapping.select_prev_item(
            { behavior = cmp.SelectBehavior.Select }),
        ['<C-u>'] = cmp.mapping.scroll_docs(-4),
        ['<C-d>'] = cmp.mapping.scroll_docs(4),
        ['<Tab>'] = cmp.mapping({
            i = function(fallback)
                -- If an option was explicitely selected, accept it.
                local entry = cmp.get_selected_entry()
                if entry ~= nil then
                    cmp.confirm({ select = false })
                    return
                end
                -- Try to expand or jump to the next tabstop
                vim.fn['UltiSnips#ExpandSnippetOrJump']()

                -- Depending on the result of the previous action, confirm
                -- the suggested autocompletion (without selecting it)
                -- or perform the default tab action.
                if vim.g.ulti_expand_or_jump_res == 0 then
                    if not cmp.visible() then
                        fallback()
                    end
                    cmp.confirm({ select = true })
                end
            end,
            s = function(fallback)
                vim.fn['UltiSnips#ExpandSnippetOrJump']()
                if vim.g.ulti_expand_or_jump_res == 0 then
                    fallback()
                end
            end
        }),
        ['<S-Tab>'] = function()
            vim.fn['UltiSnips#JumpBackwards']()
        end,
        ['<CR>'] = cmp.mapping.confirm({ select = false }),
    },

    sources = {
        { name = 'nvim_lsp' },
        { name = 'ultisnips' },
        { name = 'buffer',
            options = {
                get_bufnrs = function()
                    return vim.api.nvim_list_bufs()
                end,
            },
        },
        { name = 'path' },
    },

    formatting = {
        fields = { 'kind', 'abbr', 'menu'},
        format = function(entry, item)
            local kind_name = item.kind
            item.kind = lsp_symbols[item.kind] or ''
            item.menu = ({
                nvim_lsp = '[L]',
                ultisnips = '[S]',
                buffer = '[B]',
                path = '[P]',
            })[entry.source.name]..' '..kind_name
            return item
        end,
    },

    experimental = {
        ghost_text = {
            hl_group = 'Whitespace'
        }
    },
})
