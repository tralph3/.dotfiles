(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(set-face-attribute 'default nil
                    :font "JuliaMono-13")
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
(setq c-default-style "stroustrup")
(xterm-mouse-mode t)
(setq native-comp-async-report-warnings-errors nil)
(context-menu-mode t)
(setq mouse-drag-and-drop-region-cross-program t)

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

(if (eq (x-display-mm-width) 0)
    (progn
      (let* ((default-pixels-per-mm (/ 96.0 25.4))
             (display-mm-width (floor (+ (/ (display-pixel-width) default-pixels-per-mm) 0.5)))
             (display-mm-height (floor (+ (/ (display-pixel-height) default-pixels-per-mm) 0.5))))
        (setq display-mm-dimensions-alist `((t . (,display-mm-width . ,display-mm-height)))))

      (defun pgtk-display-monitor-attributes-list (&optional terminal)
        (let ((display-name (frame-parameter nil 'display))
              (geometry (list 0 0 (display-pixel-width terminal)
                              (display-pixel-height terminal)))
              (mm-size (list (display-mm-width terminal)
                             (display-mm-height terminal))))
          `(((name . ,display-name)
             (geometry . ,geometry)
             (workarea . ,geometry)
             (mm-size . ,mm-size)
             (scale-factor . 1.0)
             (frames . ,(frames-on-display-list terminal))
             (source . "Gdk")))))))

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
  (org-roam-capture-templates '(("d" "default" plain "%?"
                                  :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                     "#+title: ${title}\n#+filetags: :Unfinished:")
                                  :unnarrowed t)))
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
  (add-hook 'org-mode-hook 'org-superstar-mode)
  (org-superstar-mode t))

(use-package org-fragtog
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-history-directory-alist `(("." . ,(file-name-concat user-emacs-directory "undo-tree"))))
  :config
  (global-undo-tree-mode))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode t)
  :custom
  (dired-mouse-drag-files t)
  (dired-listing-switches "-lA --group-directories-first --human-readable")
  (dirvish-attributes '(vc-state subtree-state all-the-icons collapse file-time file-size))
  :config
  (dirvish-side-follow-mode t)
  :bind
  (:map dirvish-mode-map
        ("<mouse-1>" . dirvish-subtree-toggle-or-open)
        ("<mouse-2>" . dired-mouse-find-file-other-window)
        ("a"   . dirvish-quick-access)
        ("f"   . dirvish-file-info-menu)
        ("y"   . dirvish-yank-menu)
        ("N"   . dirvish-narrow)
        ("^"   . dirvish-history-last)
        ("h"   . dirvish-history-jump)
        ("s"   . dirvish-quicksort)
        ("v"   . dirvish-vc-menu)
        ("TAB" . dirvish-subtree-toggle)
        ("M-f" . dirvish-history-go-forward)
        ("M-b" . dirvish-history-go-backward)
        ("M-l" . dirvish-ls-switches-menu)
        ("M-m" . dirvish-mark-menu)
        ("M-t" . dirvish-layout-toggle)
        ("M-s" . dirvish-setup-menu)
        ("M-e" . dirvish-emerge-menu)
        ("M-j" . dirvish-fd-jump)))

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
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
(setq org-startup-with-latex-preview t)
(add-hook 'org-mode-hook 'auto-fill-mode)

(add-hook 'doc-view-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'doc-view-mode-hook (lambda () (pixel-scroll-precision-mode -1)))
(setq doc-view-scale-internally t)
(setq doc-view-continuous t)
