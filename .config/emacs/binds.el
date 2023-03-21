(defun insert-blank-line-top ()
  (interactive)
  (move-beginning-of-line nil)
  (open-line 1))

(defun insert-blank-line-bottom ()
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1))

(global-set-key (kbd "M-o") 'insert-blank-line-bottom)
(global-set-key (kbd "M-O") 'insert-blank-line-top)

(global-set-key (kbd "M-Z") 'zap-up-to-char)

(global-set-key (kbd "C-v") 'pixel-scroll-interpolate-down)
(global-set-key (kbd "M-v") 'pixel-scroll-interpolate-up)
