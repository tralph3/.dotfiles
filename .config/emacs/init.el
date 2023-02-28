(load-file (concat user-config-directory "colorscheme.el"))
(load-file (concat user-config-directory "binds.el"))
(load-file (concat user-config-directory "packages.el"))

(setq display-line-numbers-type 'relative)
(tab-line-mode 1)
(global-display-line-numbers-mode 1)
(scroll-bar-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)

(load custom-file :noerror)
