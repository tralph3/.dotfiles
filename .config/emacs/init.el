(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(push '(font . "UbuntuMono Nerd Font Mono-13") default-frame-alist)

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
  (dashboard-banner-logo-title "TitoMacs")
  (dashboard-center-content t)
  (dashboard-show-shortcuts t)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents . 5) (projects . 5) (bookmarks . 5)))
  (dashboard-heading-icons t)
  (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook))

(use-package which-key
  :ensure t
  :custom
  (which-key-idle-delay 0.2)
  :config
  (which-key-mode))

(load-file "~/.config/colorschemes/current_colorscheme/colors.el")

(push `(background-color . ,BACKGROUND_1) default-frame-alist)
(push `(foreground-color . ,FOREGROUND_1) default-frame-alist)
(set-face-background 'fringe BACKGROUND_1)

(set-face-background 'cursor ACCENT_1)

(set-face-attribute 'header-line nil
                    :background BACKGROUND_2
                    :foreground FOREGROUND_1)

(set-face-attribute 'mode-line nil
                    :background BACKGROUND_2
                    :foreground FOREGROUND_1
                    :box nil)
(set-face-attribute 'mode-line-highlight nil
                    :background ACCENT_1
                    :foreground FOREGROUND_2
                    :box nil)
(set-face-background 'mode-line-inactive BACKGROUND_1)

(set-face-attribute 'corfu-default nil
                    :background BACKGROUND_2
                    :foreground FOREGROUND_1)

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

(require 'eglot)
(setq eglot-autoshutdown t)
(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c d") 'xref-find-definitions)
(define-key eglot-mode-map (kbd "C-c h") 'eldoc)
(add-hook 'prog-mode-hook 'eglot-ensure)
