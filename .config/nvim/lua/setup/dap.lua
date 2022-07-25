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
    {
        type = 'python',
        request = 'launch',
        name = 'Test with pytest',
        module = 'pytest',
        args = { 'test' },
        pythonPath = function ()
            return '/usr/bin/python'
        end,
    },
}

dap.adapters.python = {
    type = 'executable',
    command = vim.fn.stdpath('data')..'/mason/packages/debugpy/venv/bin/python',
    args = { '-m', 'debugpy.adapter' },
}
