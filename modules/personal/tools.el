;;; tools.el --- Utility tools and editing shortcuts -*- lexical-binding: t; -*-

;;; Code:

;; --- Search ---
;; M-g opens rgrep for cross-file search with prompt for pattern + directory
(global-set-key (kbd "M-g") #'rgrep)

;; --- Date / Time Insertion ---
;; C-c i        → insert full date+time: "Thu Mar 06 14:30:00 MST 2026"
;; C-u C-c i    → insert time only:     "14:30:00"
(defun insert-current-date-time (&optional time-only)
  "Insert the current date and time. With prefix arg, insert time only."
  (interactive "P")
  (if time-only
      (insert (format-time-string "%H:%M:%S" (current-time)))
    (insert (format-time-string "%a %b %d %H:%M:%S %Z %Y" (current-time)))))

(global-set-key (kbd "C-c i") #'insert-current-date-time)

;; --- Distraction-free Writing ---
;; C-M-z toggles darkroom-tentative-mode: centers text, hides UI chrome
(use-package darkroom
  :ensure t
  :defer t)

(global-set-key (kbd "C-M-z") #'darkroom-tentative-mode)

;; --- Line Selection ---
;; s-l selects the whole line and copies it to the kill ring
(defun rsr/select-whole-line ()
  "Select the entire current line and copy it to the kill ring."
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line)
  (forward-char 1)
  (kill-ring-save (region-beginning) (region-end)))

(global-set-key (kbd "s-l") #'rsr/select-whole-line)

;; --- Comment / Uncomment ---
;; works on active region or current line if no region is selected
(defun rsr/comment-or-uncomment ()
  "Comment or uncomment the current line or active region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "s-/") #'rsr/comment-or-uncomment)
(global-set-key (kbd "C-/") #'rsr/comment-or-uncomment)

;; --- Kill Whole Line ---
(global-set-key (kbd "s-k") #'kill-whole-line)

;; --- Terminal ---
;; s-RET opens a new Kitty terminal window
(defun rsr/open-kitty ()
  "Open a new Kitty terminal window asynchronously."
  (interactive)
  (cond
   ((eq system-type 'darwin)
    (async-shell-command "open -a kitty" nil))
   ((eq system-type 'gnu/linux)
    (async-shell-command "kitty &" nil))
   (t
    (message "Unsupported OS for this function."))))

(global-set-key (kbd "s-<return>") #'rsr/open-kitty)

;; --- Org Link Storing ---
(global-set-key (kbd "C-c l") #'org-store-link)

;; --- Calculator ---
(use-package calculator
  :defer t)

(bind-keys :map rsr/global-prefix-map
           ("t c" . calc)
           ("t d" . dictionary-search))

;; --- Command Hints ---
;; which-key shows available key completions after a prefix key pause
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

;; --- Visual ---
(global-visual-line-mode t)   ;; wrap long lines visually
(show-paren-mode 1)           ;; highlight matching parentheses

(provide 'tools)
;;; tools.el ends here
