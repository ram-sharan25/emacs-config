;;; tools.el --- Utility tools and editing shortcuts -*- lexical-binding: t; -*-

;;; Code:

;;; Search
;; M-g → rgrep: cross-file search with prompt for pattern + directory

;;; Distraction-free Writing
;; C-M-z → darkroom-tentative-mode: centers text, hides UI chrome
(use-package darkroom
  :ensure t
  :defer t)

;;; Line Selection
;; s-l → select whole line and copy to kill ring
(defun rsr/select-whole-line ()
  "Select the entire current line and copy it to the kill ring."
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line)
  (forward-char 1)
  (kill-ring-save (region-beginning) (region-end)))

;;; Comment / Uncomment
;; works on active region or current line if no region is selected
(defun rsr/comment-or-uncomment ()
  "Comment or uncomment the current line or active region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;;; Terminal
;; s-RET → open a new Kitty terminal window
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

;;; Calculator
(use-package calculator
  :defer t)

;;; Command Hints
;; which-key shows available key completions after a prefix key pause
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

;;; Visual
(global-visual-line-mode t)   ;; wrap long lines visually
(show-paren-mode 1)           ;; highlight matching parentheses

;;; Keybindings

(global-set-key (kbd "M-g")        #'rgrep)
(global-set-key (kbd "C-M-z")      #'darkroom-tentative-mode)
(global-set-key (kbd "s-l")        #'rsr/select-whole-line)
(global-set-key (kbd "s-/")        #'rsr/comment-or-uncomment)
(global-set-key (kbd "C-/")        #'rsr/comment-or-uncomment)
(global-set-key (kbd "s-k")        #'kill-whole-line)
(global-set-key (kbd "s-<return>") #'rsr/open-kitty)
(global-set-key (kbd "C-c l")      #'org-store-link)

(bind-keys :map rsr/global-prefix-map
           ("t c" . calc)
           ("t d" . dictionary-search))

(provide 'tools)
;;; tools.el ends here
