local map = require('config.utils').map
local np = require('neoprojet')

np.setup({
    sessions = false,
})

local status_ok, telescope = pcall(require, 'telescope')

if status_ok then
    telescope.load_extension('neoprojet')
end

map('n', '<F5>', function() np.call_command('build_and_run') end)
map('n', 'S-<F5>', function() np.call_command('run') end)
map('n', '<F4>', function() np.call_command('run_tests') end)
