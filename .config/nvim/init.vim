" General settings
source ~/.config/nvim/configs/settings.vim
" Plugins
source ~/.config/nvim/configs/plug.vim
" Styling
source ~/.config/nvim/configs/style.vim
" Hotkeys
source ~/.config/nvim/configs/hotkeys.vim
" Various lsp configurations
luafile ~/.config/nvim/lua/lsp-configurations.lua
" Lua Line
luafile ~/.config/nvim/lua/lua-line.lua
" nvim-cmp
luafile ~/.config/nvim/lua/cmp-config.lua
" nvim-tree
luafile ~/.config/nvim/lua/neovim-tree.lua

" Show diagnostic on cursor hover
autocmd CursorHold * lua vim.diagnostic.open_float({focusable = false})
