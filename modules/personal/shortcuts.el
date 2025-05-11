(global-set-key (kbd "M-g") 'rgrep)


(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-time-format (current-time)))
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       )

(global-set-key "\C-c\C-y" 'insert-current-date-time)


(defun my-org-insert-src-block-with-extras ()
  "Insert an Org mode source block, prompting for language,
and including :results output, :exports both, and a #+RESULTS: block.
Leaves point on the line inside the source block."
  (interactive)
  (let ((language (read-string "Source code language (e.g., python): " nil nil "python")))
    (if (string-empty-p language)
	(message "No language specified. Source block not inserted.")
      (progn
	;; Insert the #+BEGIN_SRC line with language and default headers
	(insert (format "#+BEGIN_SRC %s :results output :exports both\n" language))
	;; Save point for cursor positioning, then insert the rest
	(let ((code-line-point (point)))
	  (insert "\n") ; Empty line for code
	  (insert "#+END_SRC\n\n")  ; #+END_SRC followed by two newlines
	  (insert "#+RESULTS:\n")   ; The #+RESULTS: block
	  (goto-char code-line-point) ; Move cursor to the empty line inside the SRC block
	  )))))

;; (Re)Define the keybinding specifically for Org mode
;; This ensures C-c k is only active in Org buffers.
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c k") #'my-org-insert-src-block-with-extras))
