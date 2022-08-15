(setq user-emacs-directory (expand-file-name "~/.local/share/emacs"))
(setq user-config-directory (expand-file-name "~/.config/emacs"))

(setq native-comp-eln-load-path '((. "~/.local/share/emacs/eln-cache")))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))
(setq delete-old-versions t)
(setq lock-file-name-transforms `(("^\\(.*\\)$" ,(concat user-emacs-directory "\\1") t)))
(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory))
(setq backup-directory-alist '(("." . "~/.local/share/emacs/backups")))

(load (expand-file-name "packages" user-config-directory))
(load (expand-file-name "settings" user-config-directory))
(load custom-file :noerror)
