;; manage windows in emacs

(defun ram/split-vertical ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (scratch-buffer))

(global-set-key (kbd "C-x 3") 'ram/split-vertical)

(defun ram/split-horizontal ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (scratch-buffer))

(global-set-key (kbd "C-x 2") 'ram/split-horizontal)
