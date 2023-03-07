(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package eglot
  :ensure t
  :hook (prog-mode . eglot-ensure))

(use-package company-quickhelp
  :ensure t
  :config
    (require 'company)
    (company-quickhelp-mode t))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :config (progn
    (setq company-tooltip-idle-delay 0)
    (setq company-tooltip-minimum-width 40)
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq company-selection-wrap-around t)
    (global-company-mode t)))

(use-package rust-mode
  :ensure t)

(use-package tree-sitter
  :ensure t
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode))

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
