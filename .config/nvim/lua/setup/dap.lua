local dap = require('dap')

dap.configurations.python = {
    {
        type = 'python',
        request = 'launch',
        name = 'Launch file',
        program = '${file}',
        pythonPath = function ()
            return '/usr/bin/python'
        end,
    },
}

dap.adapters.python = {
    type = 'executable',
    command = '/home/tralph3/.local/share/nvim/mason/packages/debugpy/venv/bin/python',
    args = { '-m', 'debugpy.adapter' },
}
