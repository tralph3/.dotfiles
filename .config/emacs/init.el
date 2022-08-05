(setq inhibit-startup-message t)
(setq visible-bell t)
(setq user-emacs-directory (expand-file-name "~/.local/share/emacs"))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . ,(expand-file-name "backups" user-emacs-directory))))

(load custom-file :noerror)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(hl-line-mode 1)
(blink-cursor-mode 1)

(load-theme 'deeper-blue t)

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'evil)
  (package-install 'evil))

(unless (package-installed-p 'vterm)
  (package-install 'vterm))

(unless (package-installed-p 'treemacs)
  (package-install 'treemacs))

(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons))

(require 'evil)
(evil-mode 1)

(set-face-attribute 'default nil :font "UbuntuMono Nerd Font" :height 150)
