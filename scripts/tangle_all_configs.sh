#!/bin/bash
DOTFILES_DIR=$(realpath "$(dirname "$(realpath "$0")")/..")

emacs --batch -l ob -l ob-shell --eval "
(progn
  (setq auth-sources '(\"secrets:Login\"))
  (setq dotfiles-path \"$DOTFILES_DIR\")
  (let ((org-confirm-babel-evaluate nil)
        (inhibit-message t))
    (with-current-buffer
        (find-file-noselect (file-name-concat dotfiles-path \"README.org\"))
      (org-babel-execute-buffer))))"
