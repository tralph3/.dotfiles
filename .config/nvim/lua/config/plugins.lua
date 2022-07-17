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

local function get_setup(file_name)
    return string.format('require("setup/%s")', file_name)
end

return packer.startup({
    function(use)
        use 'wbthomason/packer.nvim'

        use { 'nvim-telescope/telescope.nvim',
            config = get_setup('telescope'),
        }

        use { '/home/tralph3/projects/neoprojet/',
            config = get_setup('neoprojet'),
        }

        use 'lewis6991/impatient.nvim'

        use { 'windwp/nvim-autopairs',
            config = get_setup('autopairs'),
        }

        use 'psliwka/vim-smoothie'

        use 'tpope/vim-commentary'

        use { 'kylechui/nvim-surround',
            config = get_setup('nvim-surround'),
        }

        use 'unblevable/quick-scope'

        use { 'mrjones2014/smart-splits.nvim',
            config = get_setup('smart-splits'),
        }

        use { 'hrsh7th/nvim-cmp',
            requires = {
                'hrsh7th/cmp-buffer',
                'hrsh7th/cmp-nvim-lsp',
                'hrsh7th/cmp-path',
                { 'L3MON4D3/LuaSnip',
                    requires = {
                        'saadparwaiz1/cmp_luasnip',
                        'rafamadriz/friendly-snippets',
                    },
                    config = get_setup('luasnip'),
                },
            },
            config = get_setup('nvim-cmp'),
        }

        use { 'neovim/nvim-lspconfig',
            config = get_setup('lspconfig'),
        }


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

        use { 'norcalli/nvim-colorizer.lua',
            config = get_setup('colorizer'),
        }

        use 'kyazdani42/nvim-web-devicons'

        use { 'sunjon/Shade.nvim',
            config = get_setup('shade'),
        }

        use { 'nvim-treesitter/nvim-treesitter',
            run = ':TSUpdateSync',
            config = get_setup('treesitter'),
            requires = { 'nvim-treesitter/nvim-treesitter-textobjects' }
        }

        use 'catppuccin/nvim'

        if PACKER_JUST_INSTALLED then
            vim.api.nvim_create_autocmd('User PackerComplete', {
                command = 'qa!',
            })
            packer.sync()
        end

        pcall(require, 'impatient')
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
