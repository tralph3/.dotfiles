vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.mapleader = ' '
vim.opt.backup = false
vim.opt.colorcolumn = { 79 }
vim.opt.cursorline = true
vim.opt.expandtab = true
vim.opt.ignorecase = true
vim.opt.incsearch = true
vim.opt.laststatus = 3
vim.opt.linebreak = true
vim.opt.list = true
vim.opt.listchars = { tab = '▸ ', trail = '·' }
vim.opt.mouse = 'a'
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.scrolloff = 5
vim.opt.sessionoptions = 'buffers'
vim.opt.shiftwidth = 4
vim.opt.smartcase = true
vim.opt.smartindent = true
vim.opt.softtabstop = 4
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.tabstop = 4
vim.opt.undodir = vim.fn.expand('~/.cache/nvim/undodir')
vim.opt.undofile = true
vim.opt.updatetime = 300
vim.opt.winblend = 10
vim.opt.wrap = false

-- Vim Smoothie
vim.g.smoothie_update_interval=1

-- Quickscope
vim.g.qs_highlight_on_keys = {'f', 'F', 't', 'T'}
