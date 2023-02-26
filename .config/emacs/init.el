(setq user-emacs-directory "~/.local/share/emacs/")
(setq user-config-directory "~/.config/emacs/")

(load-file (concat user-config-directory "colorscheme.el"))
(load-file (concat user-config-directory "binds.el"))

(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)
(setq-default backup-inhibited t)
(setq-default auto-save-list-file-prefix (concat user-emacs-directory "auto-saves/.saves-"))
(setq-default custom-file (concat user-emacs-directory "custom-file"))
(setq-default package-user-dir (concat user-emacs-directory "elpa"))
(setq-default url-history-file (concat user-emacs-directory "url/history"))

(setq display-line-numbers-type 'relative)
(tab-line-mode 1)
(global-display-line-numbers-mode 1)
(scroll-bar-mode 0)

(load custom-file :noerror)
