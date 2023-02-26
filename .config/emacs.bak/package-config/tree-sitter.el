(global-tree-sitter-mode)
(push (expand-file-name "~/.local/share/treesitter-parsers/parser") tree-sitter-load-path)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
