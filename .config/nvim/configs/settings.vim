set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set nu
set nowrap
set smartcase
set noswapfile
set nobackup
set undodir=~/.cache/nvim/undodir
set undofile
set incsearch
set list
set listchars=tab:▸\ ,trail:·
set ignorecase

set number relativenumber
set colorcolumn=79
set winblend=10

let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
" Remove trailing whitespace on save
autocmd BufWritePre * :%s/\s\+$//e
