local actions = _G.actions
local map = require('config.utils').map

actions.telescope_find_files = require('telescope.builtin').find_files
actions.lsp_go_definition = require('telescope.builtin').lsp_definitions
actions.lsp_go_reference = require('telescope.builtin').lsp_references

map('n', '<leader>f', actions.telescope_find_files)
map('n', 'gd', actions.lsp_go_definition)
map('n', 'gr', actions.lsp_go_reference)

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
