(setq user-emacs-directory "~/.local/share/emacs/")
(setq user-config-directory "~/.config/emacs/")

(setq-default startup-redirect-eln-cache user-emacs-directory)
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)
(setq-default backup-inhibited t)
(setq-default auto-save-list-file-prefix (concat user-emacs-directory "auto-saves/.saves-"))
(setq-default custom-file (concat user-emacs-directory "custom-file"))
(setq-default package-user-dir (concat user-emacs-directory "elpa"))
(setq-default url-history-file (concat user-emacs-directory "url/history"))
(setq-default lock-file-name-transforms `(("^\\(.*\\)$" "/tmp/\\1") t))
(setq-default auto-save-default nil)

(setq-default pgtk-wait-for-event-timeout 0)

(setq-default gc-cons-threshold 100000000)
(setq-default read-process-output-max (* 1024 1024))
