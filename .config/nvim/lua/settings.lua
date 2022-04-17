vim.opt.backup = false
vim.opt.colorcolumn = { 79 }
vim.opt.expandtab = true
vim.opt.ignorecase = true
vim.opt.incsearch = true
vim.opt.list = true
vim.opt.listchars = { tab = '▸ ', trail = '·' }
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.shiftwidth = 4
vim.opt.smartcase = true
vim.opt.smartindent = true
vim.opt.softtabstop = 4
vim.opt.tabstop = 4
vim.opt.undodir = vim.fn.expand('~/.cache/nvim/undodir')
vim.opt.undofile = true
vim.opt.winblend = 10
vim.opt.wrap = false

-- Remove trailing whitespace on save
vim.cmd('autocmd BufWritePre * :%s/\\s\\+$//e')
-- Show diagnostics on dialog on cursor hover
vim.cmd(
    'autocmd CursorHold * lua vim.diagnostic.open_float({focusable = false})'
)

vim.cmd(
    -- TODO: Figure out how to split this so it's under the character limit
    'autocmd TextYankPost * lua vim.highlight.on_yank({igroup="IncSearch", timeout=150, on_visual=true})'
)

vim.diagnostic.config({
    virtual_text = {
        prefix = '●',
    },
})

-- Vim Smoothie
vim.g.smoothie_update_interval=1

-- Quickscope
vim.g.qs_highlight_on_keys = {'f', 'F', 't', 'T'}

-- UltiSnips
vim.g.UltiSnipsSnippetDirectories = { 'snips' }
