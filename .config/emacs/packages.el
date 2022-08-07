(defun get-config (config-name)
  (load (expand-file-name (concat "package-config/" config-name) user-config-directory)))
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(use-package evil
  :ensure t
  :config (get-config "evil"))

(use-package mood-one-theme
  :ensure t
  :config (get-config "mood-one-theme"))

(use-package centaur-tabs
  :ensure t
  :config (get-config "centaur-tabs"))

(use-package doom-modeline
  :ensure t
  :config (get-config "doom-modeline"))

(use-package vterm
  :ensure t
  :config (get-config "vterm"))

(use-package treemacs
  :ensure t
  :config (get-config "treemacs"))

(use-package all-the-icons
  :ensure t
  :config (get-config "all-the-icons"))

(use-package helm
  :ensure t
  :config (get-config "helm"))
