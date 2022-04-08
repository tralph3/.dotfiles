local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    PACKER_JUST_INSTALLED = fn.system(
        {'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path}
    )
end

local util = require('packer.util')
COMPILE_PATH = util.join_paths(
    fn.stdpath('data'), 'plugin', 'packer_compiled.lua'
)

return require('packer').startup({
    function(use)
        -- Packer
        use 'wbthomason/packer.nvim'

        -- Enhancements
        use 'jiangmiao/auto-pairs'         -- Add closing parenthesis automatically
        use 'psliwka/vim-smoothie'         -- Smooth scrolling with <C-d> and <C-u>
        use 'tpope/vim-commentary'         -- Comment lines with gc
        use 'tpope/vim-surround'           -- Surround selection with characters
        use 'unblevable/quick-scope'       -- Show hints for unique characters

        -- LSP
        use { 'hrsh7th/cmp-buffer', branch='main' }
        use { 'hrsh7th/cmp-nvim-lsp', branch='main' }
        use { 'hrsh7th/cmp-path',  branch = 'main' }
        use { 'hrsh7th/cmp-vsnip', branch = 'main' }
        use { 'hrsh7th/nvim-cmp', branch = 'main' }
        use 'hrsh7th/vim-vsnip'
        use 'neovim/nvim-lspconfig'

        -- Information
        use 'nvim-lualine/lualine.nvim'                         -- Status bar
        use { 'nvim-neo-tree/neo-tree.nvim',
            branch = 'main',
            requires = {
                "nvim-lua/plenary.nvim",
                "kyazdani42/nvim-web-devicons",
                "MunifTanjim/nui.nvim"
            }
        }

        -- Styling
        use 'sheerun/vim-polyglot'         -- Syntax highlighting for many file types
        use 'tomasiser/vim-code-dark'      -- CodeDark+ inspired colorscheme

        if PACKER_JUST_INSTALLED then
            require('packer').sync()
        end
    end,

    config= {
        compile_path = COMPILE_PATH,
        display = {
            open_fn = require('packer.util').float,
        },
        profile = {
            enabled = true,
        },
    },
}
)
