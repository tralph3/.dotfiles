" Install Plug if needed
let need_to_install_plugins = 0
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    let need_to_install_plugins = 1
endif

syntax on

" General config
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set nu
set nowrap
set smartcase
set noswapfile
set nobackup
set undodir=~/.config/nvim/undodir
set undofile
set incsearch

set number relativenumber
set colorcolumn=79
set winblend=10

" Plugins
call plug#begin(stdpath('data') . '/plugged')

Plug 'jremmen/vim-ripgrep'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'phanviet/vim-monokai-pro'
Plug 'ray-x/lsp_signature.nvim'
Plug 'timonv/vim-cargo'
Plug 'tpope/vim-commentary' " gcc comment line | gc comment motion
Plug 'tpope/vim-surround'   " ys (add surround) | ds (delete surround) | cs (change surround)
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-utils/vim-man'

call plug#end()

" Install Plug plugins
if need_to_install_plugins == 1
    echo "Installing plugins..."
    silent! PlugInstall
    echo "Done!"
    q
endif

" CoC config
let g:coc_global_extensions = [
  \ 'coc-clangd',
  \ 'coc-eslint',
  \ 'coc-godot',
  \ 'coc-godot',
  \ 'coc-html',
  \ 'coc-markdownlint',
  \ 'coc-pairs',
  \ 'coc-pyright',
  \ 'coc-rust-analyzer',
  \ 'coc-tsserver',
  \ ]

if need_to_install_plugins == 1
    echo "CoC plugins installed!"
    q
endif

" Colors
set termguicolors
colorscheme monokai_pro
let g:airline_theme = "base16"

" Background transparency
hi Normal guibg=none ctermbg=none
hi LineNr guibg=none ctermbg=none
hi Folded guibg=none ctermbg=none
hi NonText guibg=none ctermbg=none
hi SpecialKey guibg=none ctermbg=none
hi VertSplit guibg=none ctermbg=none
hi SignColumn guibg=none ctermbg=none
hi EndOfBuffer guibg=none ctermbg=none

highlight Pmenu ctermfg=NONE ctermbg=236 cterm=NONE guifg=NONE guibg=#373d48 gui=NONE
highlight PmenuSel ctermfg=NONE ctermbg=24 cterm=NONE guifg=NONE guibg=#204a87 gui=NONE

" Run ripgrep as root
if executable('rg')
    let g:rg_derive_root='true'
endif

" Config
let mapleader=" "
let g:netrw_browse_split = 2
let g:netrs_banner = 0
let g:netrs_winsize = 25

" Move lines (Alt + j/k)
nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==

" Remaps for line wrapping
nnoremap j gj
nnoremap k gk
nnoremap $ g$
nnoremap <leader>w :set wrap<CR>:set linebreak<CR>

" Switch between windows
nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>
nnoremap <leader>pv :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>
nnoremap <leader>ps :Rg<SPACE>
vmap <Tab> >gv
vmap <S-Tab> <gv

nnoremap <silent> <leader>+ :vertical resize +5<CR>
nnoremap <silent> <leader>- :vertical resize -5<CR>

" Remove trailing whitespace on save
autocmd BufWritePre * :%s/\s\+$//e
