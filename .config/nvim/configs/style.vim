" Colors
syntax on
set termguicolors
let g:codedark_italics=1
colorscheme codedark

" GUI
set guifont=UbuntuMono\ Nerd\ Font\ Mono:h12\ 12

" Background transparency
hi Normal guibg=none ctermbg=none
hi LineNr guibg=none ctermbg=none
hi Folded guibg=none ctermbg=none
hi NonText guibg=none ctermbg=none
hi SpecialKey guibg=none ctermbg=none
hi VertSplit guibg=none ctermbg=none
hi SignColumn guibg=none ctermbg=none
hi EndOfBuffer guibg=none ctermbg=none

hi Pmenu ctermfg=NONE ctermbg=236 cterm=NONE guifg=NONE guibg=#373d48 gui=NONE
hi PmenuSel ctermfg=NONE ctermbg=24 cterm=NONE guifg=NONE guibg=#204a87 gui=NONE


" Colors for the symbols in the complete menu
hi! CmpItemAbbrDeprecated guibg=NONE gui=strikethrough guifg=#808080

hi! CmpItemAbbrMatch guibg=NONE guifg=#569CD6
hi! CmpItemAbbrMatchFuzzy guibg=NONE guifg=#569CD6

hi! CmpItemKindVariable guibg=NONE guifg=#9CDCFE
hi! CmpItemKindInterface guibg=NONE guifg=#9CDCFE
hi! CmpItemKindText guibg=NONE guifg=#9CDCFE

hi! CmpItemKindFunction guibg=NONE guifg=#C586C0
hi! CmpItemKindMethod guibg=NONE guifg=#C586C0

hi! CmpItemKindKeyword guibg=NONE guifg=#D4D4D4
hi! CmpItemKindProperty guibg=NONE guifg=#D4D4D4
hi! CmpItemKindUnit guibg=NONE guifg=#D4D4D4
