;; manage windows in emacs

(defun rsr/split-vertical ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (scratch-buffer))

(global-set-key (kbd "C-x 3") 'rsr/split-vertical)

(defun rsr/split-horizontal ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (scratch-buffer))

(global-set-key (kbd "C-x 2") 'rsr/split-horizontal)
