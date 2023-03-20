(defvar emacs-smoothie-scroll-delay 0.01
  "How many seconds to wait between each scroll step.  Decrease for
faster scrolling, increase for slower scrolling. Too low values
may make the scroll appear instant, or introduce lag.")

(defvar emacs-smoothie-scroll-steps 10
  "How many scroll steps there are.")

(global-set-key (kbd "C-v") 'emacs-smoothie-scroll-page-up)
(global-set-key (kbd "M-v") 'emacs-smoothie-scroll-page-down)
(global-set-key (kbd "M-n") 'emacs-smoothie-scroll-half-page-up)
(global-set-key (kbd "M-p") 'emacs-smoothie-scroll-half-page-down)

(setq move-list (list))
(setq-default fast-but-imprecise-scrolling t)

(defun emacs-smoothie-scroll-page-down ()
  "Scrolls an entire page down. Leaves two lines of margin"
  (interactive)
  (let ((distance (* -1 (- (frame-height) 2))))
    (update-move-list distance)
    (consume-move-list)))

(defun emacs-smoothie-scroll-page-up ()
  "Scrolls an entire page up. Leaves two lines of margin"
  (interactive)
  (let ((distance (- (frame-height) 2)))
    (update-move-list distance)
    (consume-move-list)))

(defun emacs-smoothie-scroll-half-page-down ()
  "Scrolls half a page down."
  (interactive)
  (let ((distance (* -1 (/ (frame-height) 2))))
    (update-move-list distance)
    (consume-move-list)))

(defun emacs-smoothie-scroll-half-page-up ()
  "Scrolls half a page up."
  (interactive)
  (let ((distance (/ (frame-height) 2)))
    (update-move-list distance)
    (consume-move-list)))

(defun update-move-list (distance)
  "Given a distance, it add the remaining distance to be traveled
by the previous command, and generates a list of movements to get
to the new position."
  (let ((combined-distance (+ distance (apply '+ move-list))))
    (setq move-list (generate-move-list combined-distance))))

(defun generate-move-list (distance)
  "Generates a list of numbers which detail by how many lines the
buffer will scroll for each step. The numbers add up to the total
amount the screen should scroll."
  (let ((resulting-list nil)
        (result (/ distance emacs-smoothie-scroll-steps))
        (remainder (% distance emacs-smoothie-scroll-steps)))
    (dotimes (_ emacs-smoothie-scroll-steps)
      (push result resulting-list))
    (dotimes (i (abs remainder))
      (setcar (nthcdr i resulting-list) (if (>= result 0)
                                            (1+ result)
                                          (1- result))))
    resulting-list))

(defun consume-move-list ()
  "Pops element from the move-list and scrolls the buffer in the
given direction."
  (dotimes (_ emacs-smoothie-scroll-steps)
    (let ((movement (pop move-list)))
      (scroll-up-line movement)
      (sit-for emacs-smoothie-scroll-delay))))
