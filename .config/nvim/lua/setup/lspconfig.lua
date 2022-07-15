local servers = {
    -- Rust
    { 'rust_analyzer' },

    -- Python
    { 'pylsp' },
    { 'jedi_language_server' },
    { 'pyright' },

    -- Lua
    { 'sumneko_lua',
        settings = {
            Lua = {
                diagnostics = {
                    globals = { 'vim' },
                },
                telemetry = {
                    enable = false,
                },
            },
        },
    },

    -- C/C++
    { 'clangd',
        cmd = { 'clangd', '--completion-style=detailed' },
    },

    -- Godot
    { 'gdscript' },

    -- Latex
    { 'texlab' },

    -- Haskell
    { 'hls' },
}

-- Only enable the server if it's installed on the system
local lsp = require('lspconfig')
for _, server in pairs(servers) do
    local config = lsp[server[1]]

    if(lsp.util.has_bins(config.document_config.default_config.cmd[1])) then
        local opts = {}
        for k, v in pairs(server) do
            if type(k) ~= 'number' then
                opts[k] = v
            end
        end

        config.setup(opts)
    end
end
