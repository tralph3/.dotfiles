(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package eglot
  :ensure t
  :hook (prog-mode . eglot-ensure))

(use-package company
  :ensure t
  :custom
    (company-tooltip-idle-delay 0.5)
    (company-tooltip-minimum-width 40)
    (company-idle-delay 0.5)
    (company-minimum-prefix-length 1)
    (company-selection-wrap-around t)
  :config
    (global-company-mode))

(use-package company-quickhelp
  :ensure t
  :after company
  :custom
    (company-quickhelp-delay 0)
    (company-quickhelp-max-lines 20)
  :config
    (company-quickhelp-mode t))

(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode))

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
    (global-set-key (kbd "M-n") 'treemacs))

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
  :ensure t)
