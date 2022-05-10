require('utils')

-- Move lines (Alt + j/k)
-- map('n', '<A-j>', ':m .+1<CR>==')
-- map('n', '<A-k>', ':m .-2<CR>==')

-- Move entire visual selection
-- map('v', '<A-j>', ':m \'>+1<CR>gv=gv')
-- map('v', '<A-k>', ':m \'<-2<CR>gv=gv')

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
map('n', '<C-A-h>', ':vertical resize +5<CR>')
map('n', '<C-A-l>', ':vertical resize -5<CR>')

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
