-- Colors
vim.cmd('syntax on')
vim.opt.termguicolors = true

local status_ok, _ = pcall(vim.cmd, 'colorscheme catppuccin')
if not status_ok then
    vim.cmd('colorscheme slate')
end

vim.g.codedark_italics = 1

-- Diagnostic icons
vim.fn.sign_define("DiagnosticSignError",
    {text = " ", texthl = "DiagnosticSignError"}
)
vim.fn.sign_define("DiagnosticSignWarn",
    {text = " ", texthl = "DiagnosticSignWarn"}
)
vim.fn.sign_define("DiagnosticSignInfo",
    {text = " ", texthl = "DiagnosticSignInfo"}
)
vim.fn.sign_define("DiagnosticSignHint",
    {text = "", texthl = "DiagnosticSignHint"}
)

-- Font
vim.opt.guifont = 'UbuntuMono Nerd Font Mono:h12 12'

-- Quickscope
vim.cmd('hi QuickScopePrimary gui=underline guibg=green guifg=white')
vim.cmd('hi QuickScopeSecondary gui=underline guibg=red guifg=white')

-- Gitsigns
vim.cmd('hi GitSignsAdd guibg=none')
vim.cmd('hi GitSignsChange guibg=none')
vim.cmd('hi GitSignsDelete guibg=none')
