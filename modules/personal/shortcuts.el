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

(use-package darkroom
  :ensure t)
(global-set-key (kbd "C-M-z") #'darkroom-tentative-mode)

;; 1. Cmd-l to select the whole line and copy to clipboard
(defun rsr/select-whole-line ()
  "Select the entire current line and copy it to clipboard."
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line)
  (forward-char 1)  ; Include the newline character
  (kill-ring-save (region-beginning) (region-end)))

(global-set-key (kbd "s-l") #'rsr/select-whole-line)

;; 2. Cmd-/ to comment or uncomment the current line or region
(defun rsr-comment-or-uncomment ()
  "Comment or uncomment current line or region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "s-/") 'rsr-comment-or-uncomment)
(global-set-key (kbd "C-/") 'rsr-comment-or-uncomment)

;;
;; 4. Cmd-Shift-k to delete the whole line
(global-set-key (kbd "s-k") 'kill-whole-line)

(defun rsr/open-kitty ()
  "Open a new Kitty terminal window asynchronously.
This command is OS-aware, using 'open -a' on macOS
and the direct 'kitty' command on Linux."
  (interactive)
  (cond
   ((eq system-type 'darwin)  ; macOS
    (async-shell-command "open -a kitty" nil))
   ((eq system-type 'gnu/linux) ; Linux
    (async-shell-command "kitty &" nil))
   (t
    (message "Unsupported OS for this function."))))

(global-set-key (kbd "M-<return>") #'rsr/open-kitty)
(global-set-key (kbd "s-<return>") #'rsr/open-kitty)
