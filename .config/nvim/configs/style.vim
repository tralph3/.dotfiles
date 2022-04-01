" Colors
syntax on
set termguicolors
colorscheme codedark
let g:codedark_italics=1

" Get background color from colorscheme
let nvim_tree_bg_color=synIDattr(hlID('Folded'), 'bg#')

" Font
set guifont=UbuntuMono\ Nerd\ Font\ Mono:h12\ 12

" Nvim Tree
exe "hi NvimTreeNormal guibg=" . nvim_tree_bg_color
exe "hi Directory guibg=" . nvim_tree_bg_color
exe "hi NvimTreeEndOfBuffer guibg=" . nvim_tree_bg_color . " guifg=" . nvim_tree_bg_color

let g:nvim_tree_icons = {
\ 'default': " " ,
\ 'symlink': " ",
\ 'git': {
\   'unstaged': " ",
\   'staged': " ",
\   'unmerged': " ",
\   'renamed': " ",
\   'untracked': " ",
\   'deleted': " ",
\   'ignored': " "
\   },
\ 'folder': {
\   'arrow_open': "",
\   'arrow_closed': "",
\   'default': " ",
\   'open': " ",
\   'empty': " ",
\   'empty_open': " ",
\   'symlink': " ",
\   'symlink_open': " ",
\   }
\ }

" Quickscope
hi QuickScopePrimary gui=underline guibg=yellow
hi QuickScopeSecondary gui=underline guibg=red
