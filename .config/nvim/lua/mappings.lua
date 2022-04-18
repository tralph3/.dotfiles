local function map(mode, lhs, rhs, opts)
    local options = { noremap = true, silent = true }
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.keymap.set(mode, lhs, rhs, options)
end

vim.g.mapleader = " "

-- Move lines (Alt + j/k)
map('n', '<A-j>', ':m .+1<CR>==')
map('n', '<A-k>', ':m .-2<CR>==')

-- Move entire visual selection
map('v', '<A-j>', ':m \'>+1<CR>gv=gv')
map('v', '<A-k>', ':m \'<-2<CR>gv=gv')

-- Go to start and end of line
map('n', 'H', 'g^')
map('n', 'L', 'g$')

-- Paste from the yank buffer
map('n', 'yp', '"0p')
map('n', 'yP', '"0P')

-- Remaps for line wrapping
map('n', 'j', 'gj')
map('n', 'k', 'gk')
map('n', '$', 'g$')
map('n', '^', 'g^')
map('n', '<leader>w', ':set wrap<CR>:set linebreak<CR>')

-- Change buffers
map('n', 'J', ':bp<CR>')
map('n', 'K', ':bn<CR>')
map('n', '<C-w>', ':bd<CR>')

-- Format JSON file
map('n', '<leader>J', ':%!python3 -m json.tool<CR>')

-- Switch between windows
map('n', '<leader>h', ':wincmd h<CR>')
map('n', '<leader>j', ':wincmd j<CR>')
map('n', '<leader>k', ':wincmd k<CR>')
map('n', '<leader>l', ':wincmd l<CR>')

-- Indent in visual mode
map('v', '<Tab>', '>gv')
map('v', '<S-Tab>', '<gv')

-- Resize windows
map('n', '<leader>+', ':vertical resize +5<CR>')
map('n', '<leader>-', ':vertical resize -5<CR>')

-- NeoTree
map('n', '<C-n>', ':Neotree focus toggle=true<CR>')
map('n', '<C-g>', ':Neotree float toggle=true git_status<CR>')

-- Go to definition
map('n', 'gd', vim.lsp.buf.definition)
-- Go to declaration
map('n', 'gD', vim.lsp.buf.declaration)
-- List all references
map('n', 'gr', vim.lsp.buf.references)
-- Rename symbol
map('n', '<F2>', vim.lsp.buf.rename)

-- UltiSnips (proper mappings are set in the nvim-cmp config file)
vim.g.UltiSnipsExpandTrigger = '<C-x>'
vim.g.UltiSnipsJumpForwardTrigger = '<C-x>'
vim.g.UltiSnipsJumpBackwardTrigger = '<C-x>'
