local dap_ok, dap = pcall(require, 'dap')
local dapui_ok, dapui = pcall(require, 'dapui')


_G.actions = {
    next_buffer = ':bn<CR>',
    prev_buffer = ':bp<CR>',
    buffer_delete  = ':bd<CR>',
    buffer_delete_force  = ':bd!<CR>',

    change_window_left = ':wincmd h<CR>',
    change_window_right = ':wincmd l<CR>',
    change_window_up = ':wincmd k<CR>',
    change_window_down = ':wincmd j<CR>',
    close_window = ':wincmd q<CR>',
    move_window_left = ':wincmd H<CR>',
    move_window_right = ':wincmd L<CR>',
    move_window_up = ':wincmd K<CR>',
    move_window_down = ':wincmd J<CR>',

    go_start_of_line = '^',
    go_end_of_line = '$',

    paste_yanked_next = '"0p',
    paste_yanked_prev = '"0P',

    indent_in_restore_selection = '>gv',
    indent_out_restore_selection = '<gv',

    toggle_line_wrap = require('config.utils').toggle_line_wrap,

    format_json = ':%!python3 -m json.tool<CR>',

    clear_search_highlight = ':nohlsearch<CR>',

    exit_terminal_mode = '<C-\\><C-n>',

    lsp_go_definition = vim.lsp.buf.definition,
    lsp_go_reference = vim.lsp.buf.references,
    lsp_rename_symbol = vim.lsp.buf.rename,

    debug_run = "",
    debug_toggle_breakpoint = "",
    debug_step_into = "",
    debug_step_over = "",
    debug_close = "",
}

if dap_ok and dapui_ok then
    _G.actions.debug_run = dap.continue
    _G.actions.debug_toggle_breakpoint = dap.toggle_breakpoint
    _G.actions.debug_step_into = dap.step_into
    _G.actions.debug_step_over = dap.step_over
    _G.actions.debug_close = dap.terminate
end

local action_metatable = {
    __index = function(self, key)
        if self[key] == nil then
            error('attempting to access non-existent action '..key)
        else
            return self[key]
        end
    end,
}

setmetatable(_G.actions, action_metatable)
