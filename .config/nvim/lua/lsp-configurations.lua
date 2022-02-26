-- Rust
require'lspconfig'.rust_analyzer.setup{}

-- Python
require'lspconfig'.jedi_language_server.setup{}

-- Lua
require'lspconfig'.sumneko_lua.setup {
    settings = {
        Lua = {
            telemetry = {
                enable = false,
            },
        },
    },
}
