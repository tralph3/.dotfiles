local function map(mode, lhs, rhs, opts)
    local options = { noremap = true }
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

vim.g.mapleader = " "

-- Move lines (Alt + j/k)
map('n', '<A-j>', ':m .+1<CR>==', { silent = true })
map('n', '<A-k>', ':m .-2<CR>==', { silent = true })

-- Move entire visual selection
map('v', '<A-j>', ':m \'>+1<CR>gv=gv', { silent = true })
map('v', '<A-k>', ':m \'<-2<CR>gv=gv', { silent = true })

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
map('n', '<leader>w', ':set wrap<CR>:set linebreak<CR>', { silent = true })

-- Change buffers
map('n', 'J', ':bp<CR>', { silent = true })
map('n', 'K', ':bn<CR>', { silent = true })
map('n', '<C-w>', ':bd<CR>', { silent = true })

-- Format JSON file
map('n', '<leader>J', ':%!python3 -m json.tool<CR>', { silent = true })

-- Switch between windows
map('n', '<leader>h', ':wincmd h<CR>', { silent = true })
map('n', '<leader>j', ':wincmd j<CR>', { silent = true })
map('n', '<leader>k', ':wincmd k<CR>', { silent = true })
map('n', '<leader>l', ':wincmd l<CR>', { silent = true })

-- Indent in visual mode
map('v', '<Tab>', '>gv')
map('v', '<S-Tab>', '<gv')

-- Resize windows
map('n', '<leader>+', ':vertical resize +5<CR>', { silent = true })
map('n', '<leader>-', ':vertical resize -5<CR>', { silent = true })

-- Open nvim-tree
map('n', '<C-n>', ':NeoTreeFocusToggle<CR>', { silent = true })

-- Go to definition
map('n', 'gd', ':lua vim.lsp.buf.definition()<CR>', { silent = true })
-- Go to declaration
map('n', 'gD', ':lua vim.lsp.buf.declaration()<CR>', { silent = true })
-- List all references
map('n', 'gr', ':lua vim.lsp.buf.references()<CR>', { silent = true })
-- Rename symbol
map('n', '<F2>', ':lua vim.lsp.buf.rename()<CR>', { silent = true })
