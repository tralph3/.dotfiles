local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

-- Bootstrap Packer
if fn.empty(fn.glob(install_path)) > 0 then
    PACKER_JUST_INSTALLED = fn.system({
        'git', 'clone', '--depth', '1',
        'https://github.com/wbthomason/packer.nvim', install_path
    })
    vim.cmd('packadd packer.nvim')
end

local status_ok, packer = pcall(require, "packer")
if not status_ok then
    return
end

local util = require("packer.util")

COMPILE_PATH = util.join_paths(
    fn.stdpath('data'), 'plugin', 'packer_compiled.lua'
)

local function get_config(name)
    return string.format('require("setup/%s")', name)
end

return packer.startup({
    function(use)
        -- Packer
        use 'wbthomason/packer.nvim'


        -- Enhancements
        use { 'windwp/nvim-autopairs',
            config = get_config('autopairs'),
        }
        use 'psliwka/vim-smoothie'
        use 'tpope/vim-commentary'
        use 'tpope/vim-surround'
        use 'unblevable/quick-scope'


        -- LSP
        use { 'hrsh7th/nvim-cmp',
            requires = {
                'hrsh7th/cmp-buffer',
                'hrsh7th/cmp-nvim-lsp',
                'hrsh7th/cmp-path',
                'hrsh7th/cmp-vsnip',
                'hrsh7th/vim-vsnip',
            },
            config = get_config('nvim-cmp'),
        }
        use { 'neovim/nvim-lspconfig',
            config = get_config('lspconfig'),
        }


        -- Information
        use { 'nvim-lualine/lualine.nvim',
            config = get_config('lualine'),
        }
        use { 'nvim-neo-tree/neo-tree.nvim',
            requires = {
                'nvim-lua/plenary.nvim',
                'kyazdani42/nvim-web-devicons',
                'MunifTanjim/nui.nvim',
            },
            config = get_config('neo-tree'),
        }


        -- Styling
        use 'sheerun/vim-polyglot'
        use 'tomasiser/vim-code-dark'

        if PACKER_JUST_INSTALLED then
            packer.sync()
        end
    end,

    config = {
        compile_path = COMPILE_PATH,
        display = {
            open_fn = util.float,
        },
        profile = {
            enabled = true,
        },
    },
})
