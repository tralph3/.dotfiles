local mappings = require('setup.telescope.mappings')

require('telescope').setup({
    defaults = {
        mappings = mappings
    },
})
