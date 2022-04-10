-- Colors
vim.cmd('syntax on')
vim.opt.termguicolors = true

local status_ok, _ = pcall(vim.cmd, 'colorscheme codedark')
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

-- Get background color from colorscheme
local fm_color = vim.fn.synIDattr(vim.fn.hlID('Folded'), 'bg#')

-- Font
vim.opt.guifont = 'UbuntuMono Nerd Font Mono:h12 12'

-- Neo Tree
vim.cmd('hi NeoTreeNormal guibg='..fm_color)
vim.cmd('hi NeoTreeNormalNC guibg='..fm_color)
vim.cmd('hi Directory guibg='..fm_color)

-- Quickscope
vim.cmd('hi NeoTreeEndOfBuffer guibg='..fm_color..'guifg='..fm_color)
vim.cmd('hi QuickScopePrimary gui=underline guibg=green guifg=white')
vim.cmd('hi QuickScopeSecondary gui=underline guibg=red guifg=white')

-- Gitsigns
vim.cmd('hi GitSignsAdd guibg=none guifg=green')
vim.cmd('hi GitSignsDelete guibg=none guifg=red')
vim.cmd('hi GitSignsChange guibg=none guifg=blue')
