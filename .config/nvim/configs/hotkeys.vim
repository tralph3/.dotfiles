let mapleader=" "

" Move lines (Alt + j/k)
nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==

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

noremap <expr> <C-u> repeat("\<C-y> :sleep 10m<CR>", winheight('%')/2)
noremap <expr> <C-d> repeat("\<C-e> :sleep 10m<CR>", winheight('%')/2)
