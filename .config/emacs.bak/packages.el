(defun get-config (config-name)
  (let ((config-path (expand-file-name (concat "package-config/" config-name ".el") user-config-directory)))
    (if (file-exists-p config-path)
	(load config-path))))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(require 'use-package)

(use-package no-littering
  :config (setq auto-save-file-name-transforms
		`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  :config (get-config "evil"))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package centaur-tabs
  :config (get-config "centaur-tabs"))

(use-package aggressive-indent
  :config (global-aggressive-indent-mode 1))

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

(use-package tree-sitter
  :config (get-config "tree-sitter"))

(use-package tree-sitter-langs
  :config (get-config "tree-sitter-langs"))

(use-package lsp-mode
  :config (get-config "lsp-mode"))

(use-package company
  :config (get-config "company"))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package rust-mode
  :config (get-config "rust-mode"))

(use-package doom-themes
  :config (get-config "doom-themes"))

(use-package org-superstar
  :config (get-config "org-superstar"))

(use-package dap-mode
  :config (get-config "dap-mode"))
