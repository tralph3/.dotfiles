-- Colors
vim.cmd('syntax on')
vim.opt.termguicolors = true

local status_ok, _ = pcall(vim.cmd, 'colorscheme codedark')
if not status_ok then
    vim.cmd('colorscheme slate')
end

vim.g.codedark_italics = 1

-- Get background color from colorscheme
local fm_color = vim.fn.synIDattr(vim.fn.hlID('Folded'), 'bg#')

-- Font
vim.opt.guifont = 'UbuntuMono Nerd Font Mono:h12 12'

-- Neo Tree
vim.cmd('hi NeoTreeNormal guibg='..fm_color)
vim.cmd('hi Directory guibg='..fm_color)
vim.cmd('hi NeoTreeEndOfBuffer guibg='..fm_color..'guifg='..fm_color)

-- Quickscope
vim.cmd('hi NeoTreeEndOfBuffer guibg='..fm_color..'guifg='..fm_color)
vim.cmd('hi QuickScopePrimary gui=underline guibg=green guifg=white')
vim.cmd('hi QuickScopeSecondary gui=underline guibg=red guifg=white')
