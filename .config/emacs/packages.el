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
(setq use-package-always-ensure t)
(require 'use-package)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config (get-config "evil"))

(use-package centaur-tabs
  :config (get-config "centaur-tabs"))

(use-package doom-modeline
  :config (get-config "doom-modeline"))

(use-package vterm
  :config (get-config "vterm"))

(use-package all-the-icons
  :config (get-config "all-the-icons"))

(use-package treemacs
  :config (get-config "treemacs"))

(use-package treemacs-evil
  :config (get-config "treemacs-evil"))

(use-package treemacs-all-the-icons
  :config (get-config "treemacs-all-the-icons"))

(use-package helm
  :config (get-config "helm"))

(use-package tree-sitter
  :config (get-config "tree-sitter"))

(use-package tree-sitter-langs
  :config (get-config "tree-sitter-langs"))

(use-package lsp-mode
  :config (get-config "lsp-mode"))

(use-package company
  :config (get-config "company"))

(use-package rust-mode
  :config (get-config "rust-mode"))

(use-package doom-themes
  :config (get-config "doom-themes"))

(use-package org-bullets
  :config (org-bullets-mode 1))

(use-package dap-mode
  :config (get-config "dap-mode"))
