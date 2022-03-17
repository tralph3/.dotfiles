" Install plugins if needed
let need_to_install_plugins = 0
if empty(glob('~/.local/share/nvim/plugged'))
    let need_to_install_plugins = 1
endif

"Plugins
call plug#begin(stdpath('data') . '/plugged')

" Enhancements
Plug 'cskeeters/vim-smooth-scroll'  " Smooth scrolling with <C-d> and <C-u>
Plug 'jiangmiao/auto-pairs'         " Add closing parenthesis automatically
Plug 'tpope/vim-commentary'         " Comment lines with gc
Plug 'tpope/vim-surround'           " Surround selection with characters

" LSP
Plug 'hrsh7th/cmp-buffer', { 'branch': 'main' }
Plug 'hrsh7th/cmp-nvim-lsp', { 'branch': 'main' }
Plug 'hrsh7th/cmp-path', { 'branch': 'main' }
Plug 'hrsh7th/cmp-vsnip', { 'branch': 'main' }
Plug 'hrsh7th/nvim-cmp', { 'branch': 'main' }
Plug 'hrsh7th/vim-vsnip'
Plug 'neovim/nvim-lspconfig'

" Information
Plug 'nvim-lualine/lualine.nvim'    " Status bar
Plug 'kyazdani42/nvim-tree.lua'     " Integrated file manager

" Styling
Plug 'kyazdani42/nvim-web-devicons' " Icons for nvim-tree
Plug 'sheerun/vim-polyglot'         " Syntax highlighting for many file types
Plug 'tomasiser/vim-code-dark'      " CodeDark+ inspired colorscheme

call plug#end()

" Install Plug plugins
if need_to_install_plugins == 1
    echo "Installing plugins..."
    silent! PlugInstall
    echo "Done!"
    q
    q
endif
