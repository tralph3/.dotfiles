local actions = _G.actions
local map = require('config.utils').map

map('n', 'K', actions.next_buffer)
map('n', 'J', actions.prev_buffer)

map('n', '<C-w>', actions.buffer_delete)
map('n', '<C-A-w>', actions.buffer_delete_force)

map('n', '<A-h>', actions.change_window_left)
map('n', '<A-j>', actions.change_window_down)
map('n', '<A-k>', actions.change_window_up)
map('n', '<A-l>', actions.change_window_right)

map('n', '<A-w>', actions.close_window)

map('n', '<A-H>', actions.move_window_left)
map('n', '<A-K>', actions.move_window_down)
map('n', '<A-K>', actions.move_window_up)
map('n', '<A-L>', actions.move_window_right)

local status_ok, ss = pcall(require, 'smart-splits')
if status_ok then
    map('n', '<C-A-h>', ss.resize_left)
    map('n', '<C-A-j>', ss.resize_down)
    map('n', '<C-A-k>', ss.resize_up)
    map('n', '<C-A-l>', ss.resize_right)
end

map('n', 'H', actions.go_start_of_line)
map('n', 'L', actions.go_end_of_line)

map('n', 'yp', actions.paste_yanked_next)
map('n', 'yP', actions.paste_yanked_prev)

map('v', '<Tab>', actions.indent_in_restore_selection)
map('v', '<S-Tab>', actions.indent_out_restore_selection)

map('n', '<leader>w', actions.toggle_line_wrap)

map('n', '<leader>J', actions.format_json)

map('n', '<Esc>', actions.clear_search_highlight)

map('t', '<Esc>', actions.exit_terminal_mode)

map('n', 'gd', actions.lsp_go_definition)

map('n', 'gr', actions.lsp_go_reference)

map('n', '<F2>', actions.lsp_rename_symbol)
