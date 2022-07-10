local cmp = require('cmp')
local icons = _G.icons

cmp.setup({
    snippet = {
        expand = function(args)
            require('luasnip').lsp_expand(args.body)
        end,
    },

    debug = false,
    throttle_time = 80,
    source_timeout = 200,
    incomplete_delay = 400,
    max_abbr_width = 100,
    max_kind_width = 100,
    max_menu_width = 100,

    window = {
        documentation = true,
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
                local luasnip = require('luasnip')

                if entry ~= nil then
                    cmp.confirm({ select = false })
                    return
                end

                if luasnip.expand_or_jumpable() then
                    luasnip.expand_or_jump()
                elseif cmp.visible() then
                    cmp.confirm({ select = true })
                else
                    fallback()
                end
            end,
            s = function(fallback)
                local luasnip = require('luasnip')
                if luasnip.expand_or_jumpable() then
                    luasnip.expand_or_jump()
                else
                    fallback()
                end
            end
        }),
        ['<S-Tab>'] = function(fallback)
            local luasnip = require('luasnip')

            if luasnip.expand_or_jumpable() then
                luasnip.jump(-1)
            else
                fallback()
            end
        end,
        ['<CR>'] = cmp.mapping.confirm({ select = false }),
    },

    sources = {
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
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
            item.kind = icons[item.kind] or ''
            item.menu = ({
                nvim_lsp = '[L]',
                luasnip = '[S]',
                buffer = '[B]',
                path = '[P]',
            })[entry.source.name]..' '..kind_name
            return item
        end,
    },

    experimental = {
        ghost_text = {
            hl_group = 'Whitespace'
        },
    },
})
