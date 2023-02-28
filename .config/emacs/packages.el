(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package eglot
  :ensure t
  :hook (prog-mode . eglot-ensure))

(use-package company
  :ensure t
  :config (progn
	    (global-company-mode)
	    (setq company-tooltip-idle-delay 0)
	    (setq company-tooltip-minimum-width 40)
	    (setq company-idle-delay 0)
	    (setq company-minimum-prefix-length 1)
	    (setq company-selection-wrap-around t)
	    ))
