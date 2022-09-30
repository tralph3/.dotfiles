-- Colors
vim.api.nvim_command('syntax on')
vim.opt.termguicolors = true

local status_ok, _ = pcall(vim.api.nvim_command, 'colorscheme noctis')
if not status_ok then
    vim.api.nvim_command('colorscheme slate')
end

-- Diagnostics
vim.api.nvim_command('hi DiagnosticHint guibg=NONE')
vim.api.nvim_command('hi DiagnosticInfo guibg=NONE')
vim.api.nvim_command('hi DiagnosticWarn guibg=NONE')
vim.api.nvim_command('hi DiagnosticError guibg=NONE')

-- Quickscope
vim.api.nvim_command('hi QuickScopePrimary gui=underline guibg=Green guifg=White')
vim.api.nvim_command('hi QuickScopeSecondary gui=underline guibg=Red guifg=White')

-- Gitsigns
vim.api.nvim_command('hi GitSignsAdd guibg=NONE')
vim.api.nvim_command('hi GitSignsChange guibg=NONE')
vim.api.nvim_command('hi GitSignsDelete guibg=NONE')
