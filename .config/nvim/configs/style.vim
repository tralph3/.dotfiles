" Colors
syntax on
set termguicolors
let g:codedark_italics = 1
colorscheme codedark

" Font
set guifont=UbuntuMono\ Nerd\ Font\ Mono:h12

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


" Colors for the symbols in the complete menu
highlight! CmpItemAbbrDeprecated guibg=NONE gui=strikethrough guifg=#808080

highlight! CmpItemAbbrMatch guibg=NONE guifg=#569CD6
highlight! CmpItemAbbrMatchFuzzy guibg=NONE guifg=#569CD6

highlight! CmpItemKindVariable guibg=NONE guifg=#9CDCFE
highlight! CmpItemKindInterface guibg=NONE guifg=#9CDCFE
highlight! CmpItemKindText guibg=NONE guifg=#9CDCFE

highlight! CmpItemKindFunction guibg=NONE guifg=#C586C0
highlight! CmpItemKindMethod guibg=NONE guifg=#C586C0

highlight! CmpItemKindKeyword guibg=NONE guifg=#D4D4D4
highlight! CmpItemKindProperty guibg=NONE guifg=#D4D4D4
highlight! CmpItemKindUnit guibg=NONE guifg=#D4D4D4
