local dap_ok, dap = pcall(require, 'dap')
local dapui = require('dapui')
dapui.setup({})

if not dap_ok then
    return
end

dap.listeners.after.event_initialized['dapui_config'] = function()
    dapui.open()
end
dap.listeners.before.event_terminated['dapui_config'] = function()
    dapui.close()
end
dap.listeners.before.event_exited['dapui_config'] = function()
    dapui.close()
end
