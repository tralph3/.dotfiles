local servers = {
    -- Rust
    { 'rust_analyzer' },

    -- Python
    { 'jedi_language_server' },

    -- Lua
    { 'sumneko_lua',
        settings = {
            Lua = {
                telemetry = {
                    enable = false,
                },
            },
        },
    },
}

for _, server in pairs(servers) do
    local lsp = require('lspconfig')
    local config = lsp[server[1]]

    if(lsp.util.has_bins(config.document_config.default_config.cmd[1])) then
        local setup_config = {
            on_attach = on_attach,
        }

        for k, v in pairs(server) do
            if type(k) ~= 'number' then
                setup_config[k] = v
            end
        end

        config.setup(setup_config)
    end
end
