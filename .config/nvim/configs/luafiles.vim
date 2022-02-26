luafile ~/.config/nvim/lua/lsp-signature.lua
luafile ~/.config/nvim/lua/rust-lsp.lua
luafile ~/.config/nvim/lua/python-lsp.lua
luafile ~/.config/nvim/lua/lua-lsp.lua
luafile ~/.config/nvim/lua/cmp-config.lua
luafile ~/.config/nvim/lua/lua-line.lua

autocmd CursorHold * lua vim.diagnostic.open_float({focusable = false})
