;;; tools.el --- Utility tools and editing shortcuts -*- lexical-binding: t; -*-

;; Merged from shortcuts.el and tools-config.el.

;; --- Search ---

(global-set-key (kbd "M-g") #'rgrep)

;; --- Date / Time Insertion ---

(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time'.
See `format-time-string' for possible replacements.")

(defvar current-time-format "%a %H:%M:%S"
  "Format of time to insert with `insert-current-time'.")

(defun insert-current-date-time ()
  "Insert the current date and time into the current buffer."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time))))

(defun insert-current-time ()
  "Insert the current time into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time))))

(global-set-key (kbd "C-c C-y") #'insert-current-date-time)

;; --- Distraction-free Writing ---

(use-package darkroom
  :ensure t
  :defer t)

(global-set-key (kbd "C-M-z") #'darkroom-tentative-mode)

;; --- Line Selection (select + copy to kill ring) ---

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

(defun rsr-comment-or-uncomment ()
  "Comment or uncomment the current line or active region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "s-/") #'rsr-comment-or-uncomment)
(global-set-key (kbd "C-/") #'rsr-comment-or-uncomment)

;; --- Kill Whole Line ---

(global-set-key (kbd "s-k") #'kill-whole-line)

;; --- Terminal ---

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

;; --- Screenshot / OCR ---

(defun rsr/tesseract-on-file (file)
  "Run Tesseract OCR on FILE and return the extracted text as a string."
  (save-window-excursion
    (let ((buffer (generate-new-buffer "tesseract-ocr"))
          (errbuffer (generate-new-buffer "tesseract-ocr-err")))
      (shell-command (format "tesseract \"%s\" -" (file-truename file)) buffer errbuffer)
      (let ((string (with-current-buffer buffer
                      (buffer-string))))
        (kill-buffer buffer)
        (kill-buffer errbuffer)
        (remove ?\s string)))))

(defun rsr/capture-screenshot ()
  "Capture a screen region directly to the clipboard."
  (interactive)
  (call-process "screencapture" nil 0 nil "-i" "-c")
  (message "Screenshot copied to clipboard."))

(bind-keys :map rsr/global-prefix-map
           ("e o" . rsr/capture-screenshot))

;; --- Command Hints ---

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

;; --- Visual ---

(global-visual-line-mode t)
(show-paren-mode 1)

(provide 'tools)
