-- Keybinds
local list = {
    -- Enter/Exit folders
    { key = "<C-l>",       action = "cd"                   },
    { key = "<C-h>",       action = "dir_up"               },
    -- Skip between folders
    { key = "<C-k>",       action = "prev_sibling"         },
    { key = "<C-j>",       action = "next_sibling"         },
    -- Open files
    { key = "l",           action = "edit"                 },
    { key = "h",           action = "parent_node"          },
    -- View
    { key = "H",           action = "toggle_dotfiles"      },
    { key = "?",           action = "toggle_help"          },
    -- File management
    { key = "a",           action = "create"               },
    { key = "d",           action = "remove"               },
    { key = "D",           action = "trash"                },
    { key = "c",           action = "rename"               },
    { key = "C",           action = "full_rename"          },
    { key = "y",           action = "copy_path"            },
    { key = "Y",           action = "copy_absolute_path"   },
    { key = "<C-x>",       action = "cut"                  },
    { key = "<C-c>",       action = "copy"                 },
    { key = "<C-v>",       action = "paste"                },
}

require'nvim-tree'.setup{
    view = {
        mappings = {
            list = list
        }
    }
}

