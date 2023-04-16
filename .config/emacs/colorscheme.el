(require 'org-faces)
(require 'tree-sitter-hl)

(set-face-attribute 'default nil
                    :foreground FOREGROUND_1
                    :background BACKGROUND_1)
(set-face-attribute 'fringe nil
                    :foreground FOREGROUND_1
                    :background BACKGROUND_1)
(set-face-attribute 'cursor nil
                    :foreground FOREGROUND_2)

(set-face-attribute 'header-line nil
                    :background BACKGROUND_2
                    :foreground FOREGROUND_2)

(set-face-attribute 'mode-line nil
                    :background BACKGROUND_2
                    :foreground FOREGROUND_2
                    :box nil)
(set-face-attribute 'mode-line-highlight nil
                    :background HIGHLIGHT_BG
                    :foreground HIGHLIGHT_FG
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :background INACTIVE)

(set-face-attribute 'corfu-default nil
                    :background BACKGROUND_2
                    :foreground FOREGROUND_2)

(set-face-background 'org-block BACKGROUND_2)
(set-face-background 'org-block-begin-line BACKGROUND_2)
(set-face-background 'org-block-end-line BACKGROUND_2)
(set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
(set-face-attribute 'org-level-2 nil :height 1.3 :weight 'bold)
(set-face-attribute 'org-level-3 nil :height 1.1 :weight 'bold)
(set-face-attribute 'org-level-4 nil :weight 'normal)
(set-face-attribute 'org-level-5 nil :weight 'normal)
(set-face-attribute 'org-level-6 nil :weight 'normal)
(set-face-attribute 'org-level-7 nil :weight 'normal)
(set-face-attribute 'org-level-8 nil :weight 'normal)

(set-face-attribute 'font-lock-comment-face nil
                    :foreground COMMENT
                    :slant 'italic)
(set-face-attribute 'font-lock-comment-delimiter-face nil
                    :inherit 'font-lock-comment-face)
(set-face-attribute 'tree-sitter-hl-face:comment nil
                    :inherit font-lock-comment-face)

(set-face-attribute 'font-lock-keyword-face nil
                    :foreground KEYWORD)
(set-face-attribute 'tree-sitter-hl-face:keyword nil
                    :inherit 'font-lock-keyword-face)

(set-face-attribute 'font-lock-operator-face nil
                    :foreground OPERATOR)
(set-face-attribute 'tree-sitter-hl-face:operator nil
                    :inherit 'font-lock-operator-face)

(set-face-attribute 'font-lock-string-face nil
                    :foreground STRING)
(set-face-attribute 'tree-sitter-hl-face:string nil
                    :inherit 'font-lock-string-face)

(set-face-attribute 'font-lock-builtin-face nil
                    :foreground BUILTIN)
(set-face-attribute 'tree-sitter-hl-face:constant.builtin nil
                    :inherit 'font-lock-builtin-face)
(set-face-attribute 'tree-sitter-hl-face:function.builtin nil
                    :inherit 'font-lock-builtin-face)

(set-face-attribute 'font-lock-number-face nil
                    :foreground NUMBER)
(set-face-attribute 'tree-sitter-hl-face:number nil
                    :foreground NUMBER)

(set-face-attribute 'font-lock-variable-use-face nil
                    :foreground VARIABLE)
(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground VARIABLE)
(set-face-attribute 'tree-sitter-hl-face:variable nil
                    :foreground VARIABLE)
(set-face-attribute 'tree-sitter-hl-face:variable.parameter nil
                    :inherit 'tree-sitter-hl-face:variable
                    :weight 'normal)

(set-face-attribute 'font-lock-function-name-face nil
                    :foreground FUNCTION)
(set-face-attribute 'font-lock-function-call-face nil
                    :foreground FUNCTION
                    :slant 'italic)
(set-face-attribute 'tree-sitter-hl-face:function nil
                    :inherit 'font-lock-function-name-face)
(set-face-attribute 'tree-sitter-hl-face:function.call nil
                    :inherit 'font-lock-function-call-face)

(set-face-attribute 'tree-sitter-hl-face:method nil
                    :foreground METHOD)
(set-face-attribute 'tree-sitter-hl-face:method.call nil
                    :slant 'italic
                    :foreground METHOD)

(set-face-attribute 'tree-sitter-hl-face:constructor nil
                    :foreground CLASS)

(set-face-attribute 'tree-sitter-hl-face:property nil
                    :foreground ATTRIBUTE)
(set-face-attribute 'tree-sitter-hl-face:attribute nil
                    :foreground ATTRIBUTE)

(set-face-attribute 'tree-sitter-hl-face:type.parameter nil
                    :foreground PARAMETER)
(set-face-attribute 'tree-sitter-hl-face:type.argument nil
                    :foreground PARAMETER
                    :slant 'italic)

(set-face-attribute 'font-lock-constant-face nil
                    :foreground TYPE)
(set-face-attribute 'tree-sitter-hl-face:type nil
                    :foreground TYPE)
(set-face-attribute 'tree-sitter-hl-face:constant nil
                    :foreground TYPE)

(set-face-attribute 'tree-sitter-hl-face:escape nil
                    :inherit 'font-lock-string
                    :foreground KEYWORD)

(set-face-attribute 'font-lock-doc-face nil
                    :inherit 'font-lock-comment-face)
(set-face-attribute 'link nil
                    :foreground CYAN
                    :underline t)
(set-face-attribute 'link-visited nil
                    :foreground MAGENTA
                    :underline t)

(set-face-attribute 'font-lock-negation-char-face nil
                    :inherit 'font-lock-operator-face)
(set-face-attribute 'font-lock-preprocessor-face nil
                    :foreground ORANGE)

(set-face-attribute 'line-number nil
                    :foreground COMMENT)
(set-face-attribute 'line-number-current-line nil
                    :foreground FOREGROUND_1)

(set-face-attribute 'tree-sitter-hl-face:constant nil
                    :inherit 'font-lock-constant-face)



(set-face-attribute 'warning nil
                    :foreground ORANGE)
(set-face-attribute 'font-lock-warning-face nil
                    :inherit 'warning)


(set-face-attribute 'tree-sitter-hl-face:variable.parameter nil
                    :foreground PARAMETER)
(set-face-attribute 'region nil
                    :foreground HIGHLIGHT_FG
                    :background HIGHLIGHT_BG
                    :weight 'bold)
(set-face-attribute 'mode-line-highlight nil
                    :foreground HIGHLIGHT_FG
                    :background HIGHLIGHT_BG)

(set-face-attribute 'window-divider nil
                    :foreground INACTIVE)
(set-face-attribute 'vertical-border nil
                    :foreground INACTIVE)


(set-face-attribute 'show-paren-match nil
                    :background ACCENT)
(set-face-attribute 'highlight nil
                    :foreground HIGHLIGHT_FG
                    :background HIGHLIGHT_BG)
(set-face-attribute 'hl-line nil
                    :foreground HIGHLIGHT_FG
                    :background HIGHLIGHT_BG)
(set-face-attribute 'error nil
                    :foreground RED)
(set-face-attribute 'warning nil
                    :foreground ORANGE)
(set-face-attribute 'tree-sitter-hl-face:label nil
                    :foreground PARAMETER)
(set-face-attribute 'minibuffer-prompt nil
                    :foreground ACCENT)
(set-face-attribute 'success nil
                    :foreground GREEN)
(set-face-attribute 'compilation-error nil
                    :foreground RED)
(set-face-attribute 'compilation-warning nil
                    :foreground ORANGE)
(set-face-attribute 'compilation-info nil
                    :foreground BLUE)
(set-face-attribute 'compilation-mode-line-fail nil
                    :foreground RED)
(set-face-attribute 'compilation-mode-line-exit nil
                    :foreground GREEN)
(set-face-attribute 'compilation-mode-line-run nil
                    :foreground ORANGE)

(set-face-attribute 'tty-menu-disabled-face nil
                    :foreground INACTIVE
                    :background BACKGROUND_2)
(set-face-attribute 'tty-menu-selected-face nil
                    :foreground HIGHLIGHT_FG
                    :background HIGHLIGHT_BG)
(set-face-attribute 'tty-menu-enabled-face nil
                    :foreground FOREGROUND_2
                    :background BACKGROUND_2)
(setq treemacs-window-background-color `(,BACKGROUND_2 . ,HIGHLIGHT_BG))
