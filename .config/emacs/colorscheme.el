(load-file "~/.config/colorschemes/current_colorscheme/colors.el")

; BASIC COLORS
(setq-default default-frame-alist `((background-color . ,BACKGROUND_1) (foreground-color . ,FOREGROUND_1)))
(set-face-background 'fringe BACKGROUND_1)

(set-face-background 'cursor ACCENT_1)

; MODELINE
(set-face-background 'mode-line BACKGROUND_2)
(set-face-foreground 'mode-line FOREGROUND_1)
(set-face-background 'mode-line-inactive BACKGROUND_1)
(set-face-background 'mode-line-highlight ACCENT_1)
(set-face-foreground 'mode-line-highlight FOREGROUND_2)

; CORFU
(set-face-background 'corfu-default BACKGROUND_2)
(set-face-foreground 'corfu-default FOREGROUND_1)
