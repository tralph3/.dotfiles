require('config.styling.colorscheme')

local icons = _G.icons

-- Diagnostic icons
vim.fn.sign_define('DiagnosticSignError',
    { text = icons.Error, texthl = 'DiagnosticSignError' }
)
vim.fn.sign_define('DiagnosticSignWarn',
    { text = icons.Warning, texthl = 'DiagnosticSignWarn' }
)
vim.fn.sign_define('DiagnosticSignInfo',
    { text = icons.Info, texthl = 'DiagnosticSignInfo' }
)
vim.fn.sign_define('DiagnosticSignHint',
    { text = icons.Hint, texthl = 'DiagnosticSignHint' }
)

vim.diagnostic.config({
    virtual_text = {
        prefix = icons.Modified,
    },
})
