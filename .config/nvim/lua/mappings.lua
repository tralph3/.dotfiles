require('utils')

--BUFFERS-------------------------------------------

-- Change buffers
map('n', 'J', ':bp<CR>')
map('n', 'K', ':bn<CR>')

-- Delete buffers
map('n', '<C-w>', ':bd<CR>')
map('n', '<C-A-w>', ':bd!<CR>')

--WINDOWS-------------------------------------------

-- Change windows
map('n', '<A-h>', ':wincmd h<CR>')
map('n', '<A-j>', ':wincmd j<CR>')
map('n', '<A-k>', ':wincmd k<CR>')
map('n', '<A-l>', ':wincmd l<CR>')

-- Delete windows
map('n', '<A-w>', ':wincmd q<CR>')

-- Move windows
map('n', '<A-H>', ':wincmd H<CR>')
map('n', '<A-J>', ':wincmd J<CR>')
map('n', '<A-K>', ':wincmd K<CR>')
map('n', '<A-L>', ':wincmd L<CR>')

-- Resize windows
local status_ok, ss = pcall(require, 'smart-splits')
if status_ok then
    map('n', '<C-A-h>', ss.resize_left)
    map('n', '<C-A-j>', ss.resize_down)
    map('n', '<C-A-k>', ss.resize_up)
    map('n', '<C-A-l>', ss.resize_right)
end

--SHORTCUTS-----------------------------------------

-- Go to start and end of line
map('n', 'H', '^')
map('n', 'L', '$')

-- Paste from the yank buffer
map('n', 'yp', '"0p')
map('n', 'yP', '"0P')

-- Indent in visual mode
map('v', '<Tab>', '>gv')
map('v', '<S-Tab>', '<gv')

--UTILITIES-----------------------------------------

-- Line wrapping
map('n', '<leader>w', toggle_line_wrap)

-- Format JSON file
map('n', '<leader>J', ':%!python3 -m json.tool<CR>')

--LSP-----------------------------------------------

-- Go to definition
map('n', 'gd', vim.lsp.buf.definition)
-- Go to declaration
map('n', 'gD', vim.lsp.buf.declaration)
-- List all references
map('n', 'gr', vim.lsp.buf.references)
-- Rename symbol
map('n', '<F2>', vim.lsp.buf.rename)
