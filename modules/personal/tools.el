;;; tools.el --- Utility tools and editing shortcuts -*- lexical-binding: t; -*-

;;; Code:

;;; Copy / Cut
;; s-c / s-x: with no region, copy or cut the whole line (VS Code)
;; Each one starts a fresh kill-ring entry, as VS Code replaces the clipboard:
;; binding `last-command' to nil stops Emacs appending to a previous kill.
(defun rsr/copy-region-or-line ()
  "Copy the region, or the whole current line when no region is active."
  (interactive)
  (let ((last-command nil))
    (if (use-region-p)
        (call-interactively #'kill-ring-save)
      (kill-ring-save (line-beginning-position) (line-beginning-position 2))
      (message "Copied line"))))

(defun rsr/kill-region-or-line ()
  "Kill the region, or the whole current line when no region is active."
  (interactive)
  (let ((last-command nil))
    (if (use-region-p)
        (call-interactively #'kill-region)
      (kill-whole-line))))

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
;; Keep visual wrapping out of programming and special buffers.  The global
;; mode made vertical motion traverse display lines (and re-render wrapped
;; buffers) even where wrapping was not useful, which made arrow-key movement
;; feel sluggish.  Text-mode covers Org and ordinary prose buffers.
(defun rsr/enable-visual-line-for-text ()
  "Enable visual wrapping in prose buffers without changing programming modes."
  (visual-line-mode 1))
(add-hook 'text-mode-hook #'rsr/enable-visual-line-for-text)
(show-paren-mode 1)           ;; highlight matching parentheses

;;; Daily Inspiration Video
;; C-c o v → stream a random short video (<5 min) in mpv
;; Topics: eastern philosophy, Jung/Adler, math history, poems, zen, running
;; Reminded once at startup and every 45 min idle if not yet watched today.

(defvar my/video-watch-log
  (expand-file-name ".video-watched" user-emacs-directory)
  "File storing the date the last inspiration video was watched.")

(defun my/video-watched-today-p ()
  "Return non-nil if an inspiration video was already watched today."
  (and (file-exists-p my/video-watch-log)
       (string= (string-trim
                 (with-temp-buffer
                   (insert-file-contents my/video-watch-log)
                   (buffer-string)))
                (format-time-string "%Y-%m-%d"))))

(defun my/video-mark-watched ()
  "Record today's date so reminders are silenced for the rest of the day."
  (with-temp-file my/video-watch-log
    (insert (format-time-string "%Y-%m-%d"))))

(defconst my/video-ai-prompt
  "Generate one creative YouTube search query (4-8 words) for a video under 5 minutes.
The person loves: eastern philosophy (Krishnamurti, Osho, Ramana Maharshi, Vivekananda,
Vimalananda), Jungian psychology, Alfred Adler, mathematics history (how theorems and
techniques emerged and why), spoken word poetry, zen, meditation, running, personal human
stories, science discoveries, history of ideas, how people think and why they are the way
they are. Return ONLY the search query string — no quotes, no explanation, nothing else.
Be specific, creative, and vary widely each time."
  "Prompt sent to the AI to generate a YouTube search theme.")

(defvar my/video-history-file
  (expand-file-name "~/Stillness/Brain/Dashboard/video-history.org")
  "Org file where watched inspiration videos are logged.")

(defun my/video-format-duration (seconds)
  "Format SECONDS as mm:ss string."
  (let ((s (round (string-to-number (format "%s" seconds)))))
    (format "%d:%02d" (/ s 60) (% s 60))))

(defun my/video-log-entry (title url duration-secs watch-secs query)
  "Append a watch entry to `my/video-history-file'.
STATUS derived from ratio: completed >=80%, partial >=20%, skipped <20%."
  (let* ((ratio  (if (> duration-secs 0) (/ watch-secs duration-secs) 0))
         (status (cond ((>= ratio 0.8) "completed")
                       ((>= ratio 0.2) "partial")
                       (t              "skipped")))
         (entry  (format "\n* %s -- %s\n  - URL: %s\n  - Duration: %s | Watched: %s | Status: %s\n  - Query: %s\n"
                         (format-time-string "%Y-%m-%d") title url
                         (my/video-format-duration duration-secs)
                         (my/video-format-duration watch-secs)
                         status (or query "built-in"))))
    (with-temp-buffer
      (when (file-exists-p my/video-history-file)
        (insert-file-contents my/video-history-file))
      (goto-char (point-max))
      (insert entry)
      (write-region (point-min) (point-max) my/video-history-file nil 'silent))
    (message "Logged: %s (%s)" title status)))

(defun my/video-launch-script (query)
  "Run random-video.sh with optional QUERY, parse result, launch mpv from Emacs.
Script outputs ID TAB DURATION TAB TITLE.  Watch time tracked via process sentinel.
If QUERY is nil the script uses its built-in theme list."
  (let* ((script (expand-file-name "scripts/random-video.sh" user-emacs-directory))
         (cmd    (if query (list script query) (list script)))
         (output ""))
    (make-process
     :name     "random-video"
     :command  cmd
     :filter   (lambda (_proc chunk)
                 (setq output (concat output chunk)))
     :sentinel (lambda (_proc _event)
                 (let* ((line  (string-trim output))
                        (parts (split-string line "\t" t)))
                   (if (or (string-prefix-p "ERROR" line) (< (length parts) 3))
                       (if query
                           (progn (message "Query too specific, retrying...")
                                  (my/video-launch-script nil))
                         (message "Inspiration video error: %s" line))
                     (let* ((id       (nth 0 parts))
                            (duration (string-to-number (nth 1 parts)))
                            (title    (mapconcat #'identity (cddr parts) "\t"))
                            (url      (concat "https://www.youtube.com/watch?v=" id))
                            (started  (float-time)))
                       (my/video-mark-watched)
                       (message "Now watching: %s" title)
                       (let ((proc (start-process "mpv-video" nil
                                                  "/opt/homebrew/bin/mpv" url
                                                  "--geometry=900x506"
                                                  "--really-quiet")))
                         (set-process-sentinel
                          proc
                          (lambda (_p _e)
                            (my/video-log-entry
                             title url duration
                             (- (float-time) started)
                             query)))))))))))

(defun my/video-show-history ()
  "Open the video watch history file."
  (interactive)
  (find-file my/video-history-file))
(defun my/watch-random-video (&optional _arg)
  "Pick a random short inspiring video locally and stream it in mpv.
Records today as watched to suppress daily reminders."
  (interactive "P")
  (message "Finding inspiration (local)...")
  (my/video-launch-script nil))

(defun my/video-maybe-remind ()
  "Nudge if no inspiration video has been watched today."
  (unless (my/video-watched-today-p)
    (message "[Inspiration] Haven't watched today — C-c o v to get inspired.")))

(require 'org-crypt)
(setq org-tags-exclude-from-inheritance '("crypt"))
(setq org-crypt-key "rimal.ram25@gmail.com")
(setq org-crypt-disable-auto-save t)

;; Track when auto-save-visited-mode is doing its periodic save cycle,
;; so we can skip encryption (which would encrypt mid-edit every 2 sec).
(defvar rsr/auto-save-visited-in-progress nil)

(with-eval-after-load 'files
  (when (fboundp 'auto-save-visited--save-some-buffers)
    (advice-add 'auto-save-visited--save-some-buffers :around
                (lambda (fn &rest args)
                  (let ((rsr/auto-save-visited-in-progress t))
                    (apply fn args))))))

(defun rsr/org-crypt-maybe-encrypt ()
  "Encrypt :crypt: headings on explicit save only, not auto-save-visited."
  (unless rsr/auto-save-visited-in-progress
    (org-encrypt-entries)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'rsr/org-crypt-maybe-encrypt nil t)))

;; Remind 3 sec after startup (lets Emacs finish loading first)
(run-with-timer 3 nil #'my/video-maybe-remind)

;; Remind every 45 min of idle time
(run-with-idle-timer (* 45 60) t #'my/video-maybe-remind)

;;; Keybindings
;; Cmd keys, C-/ and C-c l / C-c L are in keymaps.el.

;; C-c L → store link AND push formatted [[link][desc]] to kill-ring/clipboard.
;; Lets you paste org-stored links outside Emacs (e.g. into other apps).
(defun rsr/org-store-link-to-clipboard ()
  "Run `org-store-link' and copy the result as [[link][desc]] to kill-ring."
  (interactive)
  (call-interactively #'org-store-link)
  (when org-stored-links
    (let* ((entry (car org-stored-links))
           (link  (car entry))
           (desc  (cadr entry))
           (formatted (if (and desc (not (string-empty-p desc)))
                          (format "[[%s][%s]]" link desc)
                        (format "[[%s]]" link))))
      (kill-new formatted)
      (message "Stored + copied: %s" formatted))))

(provide 'tools)
;;; tools.el ends here
