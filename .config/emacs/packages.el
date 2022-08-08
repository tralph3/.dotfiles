(defun get-config (config-name)
  (let ((config-path (expand-file-name (concat "package-config/" config-name ".el") user-config-directory)))
    (if (file-exists-p config-path)
	(load config-path))))
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(use-package evil
  :ensure t
  :config (get-config "evil"))

(use-package centaur-tabs
  :ensure t
  :config (get-config "centaur-tabs"))

(use-package doom-modeline
  :ensure t
  :config (get-config "doom-modeline"))

(use-package vterm
  :ensure t
  :config (get-config "vterm"))

(use-package treemacs
  :ensure t
  :after all-the-icons
  :config (get-config "treemacs"))

(use-package treemacs-evil
  :ensure t
  :config (get-config "treemacs-evil"))

(use-package treemacs-all-the-icons
  :ensure t
  :config (get-config "treemacs-all-the-icons"))

(use-package all-the-icons
  :ensure t
  :config (get-config "all-the-icons"))

(use-package helm
  :ensure t
  :config (get-config "helm"))

(use-package tree-sitter
  :ensure t
  :config (get-config "tree-sitter"))

(use-package tree-sitter-langs
  :ensure t
  :config (get-config "tree-sitter-langs"))

(use-package lsp-mode
  :ensure t
  :config (get-config "lsp-mode"))

(use-package company
  :ensure t
  :config (get-config "company"))

(use-package rust-mode
  :ensure t
  :config (get-config "rust-mode"))

(use-package doom-themes
  :ensure t
  :config (get-config "doom-themes"))
