(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package eglot
  :ensure t
  :hook (prog-mode . eglot-ensure))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 0)
  (corfu-min-width 60)
  (corfu-popupinfo-delay 0.5)
  :config
  (corfu-popupinfo-mode)
  (global-corfu-mode))

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

(use-package tree-sitter
  :ensure t
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package treemacs
  :ensure t
  :config
    (require 'treemacs-all-the-icons)
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
    (treemacs-project-follow-mode t)
    (treemacs-fringe-indicator-mode -1))

(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs
  :config
    (treemacs-load-theme "all-the-icons"))

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
