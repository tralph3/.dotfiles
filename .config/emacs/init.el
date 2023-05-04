(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(set-face-attribute 'default nil
                    :font "UbuntuMono Nerd Font Mono-13")
(set-face-attribute 'variable-pitch nil
                    :font "Ubuntu-13")

(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq inhibit-startup-screen t)
(delete-selection-mode t)
(electric-pair-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)
(xterm-mouse-mode t)
(setq native-comp-async-report-warnings-errors nil)
(context-menu-mode t)

(setq-default scroll-conservatively 10000)
(setq-default scroll-margin 5)

(setq pixel-scroll-precision-use-momentum t)
(setq pixel-scroll-precision-interpolate-mice t)
(setq pixel-scroll-precision-large-scroll-height 10.0)
(setq pixel-scroll-precision-interpolate-page t)
(pixel-scroll-precision-mode t)

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

(use-package all-the-icons
  :ensure t)

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
(define-key corfu-map (kbd "M-SPC") 'corfu-insert-separator)

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

(defun project-open (project-root)
  (cd project-root)
  (treemacs-add-and-display-current-project-exclusively))

(defun open-config ()
  (interactive)
  (let ((dotfiles-dir (getenv"DOTFILES_DIR")))
    (cd (file-name-concat dotfiles-dir "/.config"))
    (treemacs-select-directory)))

(global-set-key (kbd "C-c c") 'open-config)

(use-package treemacs
  :ensure t
  :custom
  (treemacs-read-string-input 'from-minibuffer)
  :config
  (treemacs-fringe-indicator-mode -1))

(use-package dashboard
  :ensure t
  :after all-the-icons
  :custom
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-banner-logo-title "TitoMacs")
  (dashboard-center-content t)
  (dashboard-show-shortcuts t)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents . 5) (projects . 5) (bookmarks . 5)))
  (dashboard-startup-banner 'logo)
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   `(((,(all-the-icons-octicon "file-text" :height 1.0 :v-adjust 0.0)
       "Emacs Config"
       "Open the Emacs config file"
       (lambda (&rest _)
         (find-file (file-name-concat user-config-directory "README.org")))))))
  (dashboard-projects-switch-function 'project-open)
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

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(setq zettelkasten-paths-alist '(("Main" . "~/Documents/wiki/")
                                 ("NesWiki" . "~/Documents/NesWiki/")))

(defun switch-zettelkasten ()
  (interactive)
  (let* ((keys (mapcar #'car zettelkasten-paths-alist))
         (prompt (format "Select Zettelkasten:"))
         (key (completing-read prompt keys))
         (chosen-zettelkasten-path (cdr (assoc key zettelkasten-paths-alist))))
    (setq org-roam-directory chosen-zettelkasten-path)
    (setq org-roam-db-location (file-name-concat chosen-zettelkasten-path "org-roam.db"))
    (org-roam-db-sync)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (cdr (assoc-string "Main" zettelkasten-paths-alist)))
  (org-roam-db-location (file-name-concat (cdr (assoc-string "Main" zettelkasten-paths-alist)) "org-roam.db"))
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n s" . switch-zettelkasten)
   (:map org-mode-map
         (("C-c n i" . org-roam-node-insert)
          ("C-c n t" . org-roam-tag-add)
          ("C-c n a" . org-roam-alias-add)
          ("C-c n b" . org-roam-buffer-toggle))))
  :config
  (org-roam-db-autosync-mode t))

(use-package org-roam-ui
  :ensure t
  :custom
  (org-roam-ui-open-on-start nil)
  (org-roam-ui-sync-theme nil))

(use-package org-superstar
  :ensure t
  :custom
  (org-superstar-item-bullet-alist '((42 . 8226)
                                     (43 . 8226)
                                     (45 . 8211)))
  :config
  (org-superstar-mode t))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(defun reload-colorscheme ()
  (interactive)
  (load "~/.config/colorschemes/current_colorscheme/colors.el" 'noerror 'nomessage)
  (load (file-name-concat user-config-directory "colorscheme.el") 'noerror 'nomessage)
  (treemacs-realign-icon-colors)
  (ignore-errors
    (org-roam-ui-sync-theme)))

(define-key special-event-map [sigusr1] 'reload-colorscheme)
(reload-colorscheme)

(require 'eglot)
(setq eglot-autoshutdown t)
(setq eglot-sync-connect 0)
(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(global-set-key (kbd "C-c d") 'xref-find-definitions)
(global-set-key (kbd "C-c h") 'eldoc)
(global-set-key (kbd "C-c b") 'xref-go-back)
(global-set-key (kbd "C-c R") 'xref-find-references)
(add-hook 'prog-mode-hook 'eglot-ensure)

(require 'org-tempo)
(setq org-startup-indented t)
(setq org-pretty-entities t)
(setq org-hide-emphasis-markers t)
(setq org-startup-with-inline-images t)
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-preview-latex-image-directory "~/.cache/ltximg")
(add-hook 'org-mode-hook 'org-superstar-mode)
