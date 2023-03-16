(load-file (concat user-config-directory "colorscheme.el"))
(load-file (concat user-config-directory "binds.el"))
(load-file (concat user-config-directory "packages.el"))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default truncate-lines t)
(setq inhibit-startup-screen t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(load custom-file :noerror)
