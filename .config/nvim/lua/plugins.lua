local fn = vim.fn
local data_path = fn.stdpath('data')
local compile_path = data_path..'/site/plugin/packer_compiled.lua'
local install_path = data_path..'/site/pack/packer/start/packer.nvim'

-- Bootstrap Packer
if fn.empty(fn.glob(install_path)) > 0 then
    PACKER_JUST_INSTALLED = fn.system({
        'git', 'clone', '--depth', '1',
        'https://github.com/wbthomason/packer.nvim', install_path
    })
    vim.cmd('packadd packer.nvim')
end

local status_ok, packer = pcall(require, 'packer')
if not status_ok then
    return
end

local function get_setup(name)
    return string.format('require("setup/%s")', name)
end

return packer.startup({
    function(use)
        -- Packer
        use 'wbthomason/packer.nvim'


        -- Enhancements
        use { 'windwp/nvim-autopairs',
            config = get_setup('autopairs'),
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
            config = get_setup('nvim-cmp'),
        }
        use { 'neovim/nvim-lspconfig',
            config = get_setup('lspconfig'),
        }


        -- Information
        use { 'nvim-lualine/lualine.nvim',
            config = get_setup('lualine'),
        }
        use { 'nvim-neo-tree/neo-tree.nvim',
            requires = {
                'nvim-lua/plenary.nvim',
                'MunifTanjim/nui.nvim',
            },
            config = get_setup('neo-tree'),
        }
        use { 'lewis6991/gitsigns.nvim',
            config = get_setup('gitsigns'),
        }


        -- Styling
        use { 'norcalli/nvim-colorizer.lua',
            config = get_setup('colorizer'),
        }
        use 'kyazdani42/nvim-web-devicons'
        use { 'nvim-treesitter/nvim-treesitter',
            run = ':TSUpdate',
            config = get_setup('treesitter'),
        }
        use 'tomasiser/vim-code-dark'

        if PACKER_JUST_INSTALLED then
            packer.sync()
        end
    end,
    config = {
        compile_path = compile_path,
        display = {
            open_fn = require('packer.util').float,
        },
        profile = {
            enabled = true,
        },
    },
})
