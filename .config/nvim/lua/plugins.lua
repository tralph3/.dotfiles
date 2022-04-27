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

        -- Improves startup time
        use 'lewis6991/impatient.nvim'

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
                'quangnguyen30192/cmp-nvim-ultisnips',
                'sirver/UltiSnips',
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
            branch = 'v2.x',
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
        use { 'sunjon/Shade.nvim',
            config = get_setup('shade'),
        }
        use { 'nvim-treesitter/nvim-treesitter',
            run = ':TSUpdate',
            config = get_setup('treesitter'),
            requires = { 'nvim-treesitter/nvim-treesitter-textobjects' }
        }
        use 'catppuccin/nvim'

        if PACKER_JUST_INSTALLED then
            packer.sync()
        end

        require('impatient')
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
