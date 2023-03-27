(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(set-frame-font "UbuntuMono Nerd Font Mono-13")

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq inhibit-startup-screen t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default scroll-conservatively 10000)
(setq-default scroll-margin 5)

(pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-use-momentum t)
(setq pixel-scroll-precision-interpolate-mice t)
(setq pixel-scroll-precision-large-scroll-height 10.0)
(setq pixel-scroll-precision-interpolate-page t)

(global-set-key (kbd "C-v") 'pixel-scroll-interpolate-down)
(global-set-key (kbd "M-v") 'pixel-scroll-interpolate-up)

(defun insert-blank-line-top ()
  (interactive)
  (move-beginning-of-line nil)
  (open-line 1))

(defun insert-blank-line-bottom ()
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1))

(global-set-key (kbd "M-o") 'insert-blank-line-bottom)
(global-set-key (kbd "M-O") 'insert-blank-line-top)

(global-set-key (kbd "M-Z") 'zap-up-to-char)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 0)
  (corfu-min-width 60)
  (corfu-popupinfo-delay 0.5)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match t)
  (corfu-on-exact-match 'quit)
  (corfu-cycle t)
  :config
  (corfu-popupinfo-mode)
  (global-corfu-mode))

(defun corfu-handle-tab-completion ()
  (interactive)
  (if (>= corfu--index 0)
      (corfu-complete)
    (progn
      (setq corfu--index 0)
      (corfu-complete))))

(defun corfu-handle-return-completion ()
  (interactive)
  (if (>= corfu--index 0)
      (corfu-complete)
    (newline)))

(setq corfu-map (make-sparse-keymap))
(define-key corfu-map (kbd "M-n") 'corfu-next)
(define-key corfu-map (kbd "M-p") 'corfu-previous)
(define-key corfu-map (kbd "TAB") 'corfu-handle-tab-completion)
(define-key corfu-map (kbd "RET") 'corfu-handle-return-completion)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package rust-mode
  :ensure t)

(use-package vertico
  :ensure t
  :config
  (vertico-mode)
  (vertico-mouse-mode))

(use-package magit
  :ensure t
  :bind (("C-c g" . 'magit-status)))

(use-package orderless
  :ensure t
  :config
  (add-to-list 'completion-styles 'orderless))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-banner-logo-title "TitoMacs")
  (dashboard-center-content t)
  (dashboard-show-shortcuts t)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents . 5) (projects . 5) (bookmarks . 5)))
  (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook))

(use-package tree-sitter
  :ensure t
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package which-key
  :ensure t
  :custom
  (which-key-idle-delay 0.2)
  :config
  (which-key-mode))

(defun reload-colorscheme ()
  (interactive)
(load-file "~/.config/colorschemes/current_colorscheme/colors.el")

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
(set-face-background 'mode-line-inactive INACTIVE)

(set-face-attribute 'corfu-default nil
                    :background BACKGROUND_2
                    :foreground FOREGROUND_2)

(require 'org-faces)
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

(require 'tree-sitter-hl)
(set-face-attribute 'font-lock-keyword-face nil
                    :foreground KEYWORD)
(set-face-attribute 'font-lock-comment-face nil
                    :foreground COMMENT
                    :slant 'italic)
(set-face-attribute 'font-lock-operator-face nil
                    :foreground OPERATOR)
(set-face-attribute 'font-lock-string-face nil
                    :foreground STRING)
(set-face-attribute 'font-lock-number-face nil
                    :foreground NUMBER)
(set-face-attribute 'font-lock-variable-use-face nil
                    :foreground VARIABLE)
(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground VARIABLE)
(set-face-attribute 'font-lock-function-name-face nil
                    :foreground FUNCTION)
(set-face-attribute 'font-lock-function-call-face nil
                    :foreground FUNCTION)
(set-face-attribute 'font-lock-builtin-face nil
                    :foreground BUILTIN)
(set-face-attribute 'font-lock-comment-delimiter-face nil
                    :inherit 'font-lock-comment-face)
(set-face-attribute 'font-lock-constant-face nil
                    :foreground VARIABLE)
(set-face-attribute 'font-lock-doc-face nil
                    :inherit 'font-lock-comment-face)
(set-face-attribute 'link nil
                    :foreground CYAN
                    :underline t)
(set-face-attribute 'link-visited nil
                    :foreground MAGENTA
                    :underline t)
(set-face-attribute 'tree-sitter-hl-face:function.call nil
                    :inherit 'font-lock-function-call-face)
(set-face-attribute 'font-lock-negation-char-face nil
                    :inherit 'font-lock-operator-face)
(set-face-attribute 'font-lock-preprocessor-face nil
                    :foreground ORANGE)
(set-face-attribute 'tree-sitter-hl-face:comment nil
                    :inherit font-lock-comment-face)
(set-face-attribute 'line-number nil
                    :foreground COMMENT)
(set-face-attribute 'line-number-current-line nil
                    :foreground FOREGROUND_1)
(set-face-attribute 'tree-sitter-hl-face:attribute nil
                    :inherit 'font-lock-constant-face)
(set-face-attribute 'tree-sitter-hl-face:constant nil
                    :inherit 'font-lock-constant-face)
(set-face-attribute 'tree-sitter-hl-face:constant.builtin nil
                    :inherit 'font-lock-builtin-face)
(set-face-attribute 'tree-sitter-hl-face:constructor nil
                    :inherit 'font-lock-constant-face)
(set-face-attribute 'tree-sitter-hl-face:escape nil
                    :inherit 'font-lock-string
                    :foreground KEYWORD)
(set-face-attribute 'warning nil
                    :foreground ORANGE)
(set-face-attribute 'font-lock-warning-face nil
                    :inherit 'warning)
(set-face-attribute 'tree-sitter-hl-face:function nil
                    :inherit 'font-lock-function-name-face)
(set-face-attribute 'tree-sitter-hl-face:function.builtin nil
                    :inherit 'font-lock-builtin-face)
(set-face-attribute 'tree-sitter-hl-face:function.call nil
                    :inherit 'font-lock-function-name-face
                    :weight 'normal)
(set-face-attribute 'tree-sitter-hl-face:keyword nil
                    :inherit 'font-lock-keyword-face)
(set-face-attribute 'tree-sitter-hl-face:string nil
                    :inherit 'font-lock-string-face)
(set-face-attribute 'tree-sitter-hl-face:type.parameter nil
                    :foreground PARAMETER)
(set-face-attribute 'tree-sitter-hl-face:variable nil
                    :foreground VARIABLE)
(set-face-attribute 'tree-sitter-hl-face:variable.parameter nil
                    :inherit 'tree-sitter-hl-face:variable
                    :weight 'normal)
(set-face-attribute 'tree-sitter-hl-face:method nil
                    :foreground METHOD)
(set-face-attribute 'tree-sitter-hl-face:method.call nil
                    :slant 'italic
                    :foreground FUNCTION)
(set-face-attribute 'tree-sitter-hl-face:type.argument nil
                    :foreground PARAMETER
                    :slant 'italic)
(set-face-attribute 'tree-sitter-hl-face:constructor nil
                    :foreground CLASS)
(set-face-attribute 'tree-sitter-hl-face:variable.parameter nil
                    :foreground PARAMETER)
(set-face-attribute 'region nil
                    :foreground HIGHLIGHT_FG
                    :background HIGHLIGHT_BG)
(set-face-attribute 'mode-line-highlight nil
                    :foreground HIGHLIGHT_FG
                    :background HIGHLIGHT_BG)
(set-face-attribute 'tree-sitter-hl-face:property nil
                    :foreground ATTRIBUTE)
(set-face-attribute 'window-divider nil
                    :foreground INACTIVE)
(set-face-attribute 'vertical-border nil
                    :foreground INACTIVE)
(set-face-attribute 'tree-sitter-hl-face:type nil
                    :foreground TYPE)
(set-face-attribute 'tree-sitter-hl-face:constant nil
                    :foreground TYPE)
(set-face-attribute 'tree-sitter-hl-face:operator nil
                    :inherit 'font-lock-operator-face)
(set-face-attribute 'show-paren-match nil
                    :background ACCENT)
(set-face-attribute 'highlight nil
                    :foreground HIGHLIGHT_FG
                    :background HIGHLIGHT_BG)

) ; closes the function
(reload-colorscheme)
(define-key special-event-map [sigusr1] 'reload-colorscheme)

(require 'eglot)
(setq eglot-autoshutdown t)
(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(global-set-key (kbd "C-c d") 'xref-find-definitions)
(define-key eglot-mode-map (kbd "C-c h") 'eldoc)
(add-hook 'prog-mode-hook 'eglot-ensure)
