vim.opt.backup = false
vim.opt.colorcolumn = { 79 }
vim.opt.expandtab = true
vim.opt.ignorecase = true
vim.opt.incsearch = true
vim.opt.laststatus = 3
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

vim.diagnostic.config({
    virtual_text = {
        prefix = '●',
    },
})

-- Remove trailing whitespace on save
vim.api.nvim_create_autocmd('BufWritePre', {
    command = '%s/\\s\\+$//e',
})

-- Show diagnostics on dialog on cursor hover
vim.api.nvim_create_autocmd('CursorHold', {
    callback = function()
        vim.diagnostic.open_float({ focusable = false })
    end,
})

-- Highlight yanked text
vim.api.nvim_create_autocmd('TextYankPost', {
    callback = function()
        vim.highlight.on_yank(
            { igroup="IncSearch", timeout=150, on_visual=true }
        )
    end,
})

-- Vim Smoothie
vim.g.smoothie_update_interval=1

-- Quickscope
vim.g.qs_highlight_on_keys = {'f', 'F', 't', 'T'}

-- UltiSnips
vim.g.UltiSnipsSnippetDirectories = { 'snips' }
