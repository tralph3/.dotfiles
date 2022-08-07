(global-tree-sitter-mode)
(push (expand-file-name "~/.local/share/treesitter-parsers/parser") tree-sitter-load-path)
(tree-sitter-require 'python)
