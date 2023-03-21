(load-file (concat user-config-directory "binds.el"))
(load-file (concat user-config-directory "packages.el"))
(load-file (concat user-config-directory "colorscheme.el"))

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
(push '(font . "UbuntuMono Nerd Font Mono-13") default-frame-alist)
(setq-default scroll-conservatively 10000)
(setq-default scroll-margin 5)

(pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-use-momentum t)
(setq pixel-scroll-precision-interpolate-mice t)
(setq pixel-scroll-precision-large-scroll-height 10.0)
(setq pixel-scroll-precision-interpolate-page t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
