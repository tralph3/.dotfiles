let mapleader=" "

" Move lines (Alt + j/k)
nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==

" Move entire visual selection
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv

" Go to start and end of line
nnoremap H ^
nnoremap L $

" Remaps for line wrapping
nnoremap j gj
nnoremap k gk
nnoremap $ g$
nnoremap <leader>w :set wrap<CR>:set linebreak<CR>

" Change buffers
nnoremap J :bp<CR>
nnoremap K :bn<CR>
nnoremap <C-w> :bd<CR>

" Format JSON file
nnoremap <leader>J :%!python -m json.tool<CR>

" Switch between windows
nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>

" Indent in visual mode
vmap <Tab> >gv
vmap <S-Tab> <gv

" Resize windows
nnoremap <silent> <leader>+ :vertical resize +5<CR>
nnoremap <silent> <leader>- :vertical resize -5<CR>

" Mirror the NERDTree before showing it. This makes it the same on all tabs.
nnoremap <silent> <C-n> :NERDTreeToggle<CR>:NERDTreeMirror<CR>

" Go to definition
nnoremap gd :lua vim.lsp.buf.definition()<CR>
" Go to declaration
nnoremap gD :lua vim.lsp.buf.declaration()<CR>
" List all references
nnoremap gr :lua vim.lsp.buf.references()<CR>
" Rename symbol
nnoremap <F2> :lua vim.lsp.buf.rename()<CR>
