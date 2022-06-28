local icons = _G.icons
local map = require('config.utils').map

vim.g.neo_tree_remove_legacy_commands = 1

map('n', '<C-n>', ':Neotree focus toggle=true<CR>')

require('neo-tree').setup({
    close_if_last_window = false,
    close_floats_on_escape_key = true,
    default_source = 'filesystem',
    enable_diagnostics = true,
    enable_git_status = true,
    git_status_async = true,
    log_level = 'info',
    log_to_file = false,
    open_files_in_last_window = true,
    popup_border_style = 'rounded',
    resize_timer_interval = 50,
    sort_case_insensitive = true,
    use_popups_for_input = false,
    default_component_configs = {
        icon = {
            folder_closed = icons.Folder,
            folder_open = icons.FolderOpen,
            folder_empty = icons.FolderEmpty,
            default = icons.File,
        },
        modified = {
            symbol = icons.Modified,
            highlight = 'NeoTreeModified',
        },
        git_status = {
            symbols = {
                added     = '',
                deleted   = '',
                modified  = '',
                renamed   = '',
                untracked = '',
                ignored   = '',
                unstaged  = '',
                staged    = '',
                conflict  = '',
            },
            align = 'right',
        },
    },
    window = {
        mappings = {
            ['<space>'] = '',
            ['<2-LeftMouse>'] = '',
            ['<CR>'] = 'open',
            ['l'] = 'open',
            ['S'] = 'open_split',
            ['s'] = 'open_vsplit',
            ['t'] = '',
            ['w'] = '',
            ['C'] = 'close_node',
            ['z'] = 'close_all_nodes',
            ['R'] = 'refresh',
            ['a'] = 'add',
            ['A'] = 'add_directory',
            ['d'] = 'delete',
            ['r'] = 'rename',
            ['y'] = 'copy_to_clipboard',
            ['x'] = 'cut_to_clipboard',
            ['p'] = 'paste_from_clipboard',
            ['c'] = 'copy',
            ['m'] = 'move',
            ['q'] = 'close_window',
        },
    },
    filesystem = {
        window = {
            mappings = {
                ['H'] = 'toggle_hidden',
                ['/'] = '',
                ['f'] = '',
                ['<C-x>'] = 'clear_filter',
                ['<C-h>'] = 'navigate_up',
                ['<C-l>'] = 'set_root',
            }
        },
        async_directory_scan = 'auto',
        bind_to_cwd = true,
        filtered_items = {
            hide_gitignored = false,
        },
        follow_current_file = true,
        hijack_netrw_behavior = 'open_default',
        use_libuv_file_watcher = true,
    },
    buffers = {
        bind_to_cwd = true,
        window = {
            mappings = {
                ['<C-h>'] = 'navigate_up',
                ['<C-l>'] = 'set_root',
                ['d'] = 'buffer_delete',
            },
        },
    },
    git_status = {
        window = {
            mappings = {
                ['gA'] = 'git_add_all',
                ['gu'] = 'git_unstage_file',
                ['ga'] = 'git_add_file',
                ['gr'] = 'git_revert_file',
                ['gc'] = 'git_commit',
                ['gp'] = 'git_push',
                ['gg'] = 'git_commit_and_push',
            },
        },
    },
})

