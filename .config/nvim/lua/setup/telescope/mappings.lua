local actions = require('config.globals').actions
local map = require('config.utils').map

actions.telescope_find_files = ':Telescope find_files<CR>'
actions.lsp_go_definition = ':Telescope lsp_definitions<CR>'
actions.lsp_go_reference = ':Telescope lsp_references<CR>'

map('n', '<leader>f', actions.telescope_find_files)

local mappings = {
    i = {
        ['<C-j>'] = 'move_selection_next',
        ['<C-k>'] = 'move_selection_previous',
    },
    n = {
        ['<C-j>'] = 'move_selection_next',
        ['<C-k>'] = 'move_selection_previous',
    },
}

return mappings
