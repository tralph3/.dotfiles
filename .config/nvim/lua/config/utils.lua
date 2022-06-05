local M = {}

M.toggle_line_wrap = function()
    vim.opt.wrap = not(vim.opt.wrap:get())
end

M.map = function(mode, lhs, rhs, opts)
    local options = { noremap = true, silent = true }
    options = vim.tbl_extend("force", options, opts or {})
    vim.keymap.set(mode, lhs, rhs, options)
end

return M
