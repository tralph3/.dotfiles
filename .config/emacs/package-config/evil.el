(setq evil-undo-system 'undo-redo)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map "L" 'evil-end-of-line)
(evil-mode t)
