vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.wrap = false
vim.opt.smartcase = true
vim.opt.backup = false
vim.opt.undodir = '~/.cache/nvim/undodir'
vim.opt.incsearch = true
vim.opt.list = true
vim.opt.listchars = { tab = '▸ ', trail = '·' }
vim.opt.ignorecase = true
vim.opt.colorcolumn = { 79 }
vim.opt.winblend = 10

-- Remove trailing whitespace on save
vim.cmd('autocmd BufWritePre * :%s/\\s\\+$//e')
-- Show diagnostics on dialog on cursor hover
vim.cmd(
    'autocmd CursorHold * lua vim.diagnostic.open_float({focusable = false})'
)

vim.g.smoothie_update_interval=1
vim.g.qs_highlight_on_keys = {'f', 'F', 't', 'T'}
