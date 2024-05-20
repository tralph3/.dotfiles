#! /bin/bash
emacs --batch -l ob -l ob-shell --eval "
(progn
  (setq auth-sources '(\"secrets:Login\"))
  (let ((org-confirm-babel-evaluate nil))
    (with-current-buffer
        (find-file-noselect (file-name-concat (getenv \"DOTFILES_DIR\") \"README.org\"))
      (org-babel-execute-buffer))))"
