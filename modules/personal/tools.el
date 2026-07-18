;;; tools.el --- Utility tools and editing shortcuts -*- lexical-binding: t; -*-

;;; Code:

;;; Search
;; M-g → rgrep: cross-file search with prompt for pattern + directory

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

;;; Keybinding Search
;; C-c o s → search keybindings.md with live consult-line
(defun rsr/search-keybindings ()
  "Search all keybindings by keyword in the minibuffer.

Parses keybindings.md and prefixes each table row with its section heading
so you can search by topic (e.g. \"roam\", \"capture\") or by key.
No file buffer is opened or switched to."
  (interactive)
  (let* ((file (expand-file-name "docs/reference/keybindings.md" user-emacs-directory))
         (lines (with-temp-buffer
                  (insert-file-contents file)
                  (split-string (buffer-string) "\n")))
         (section "")
         candidates)
    ;; Build candidates: track headings, annotate each data row with section
    (dolist (line lines)
      (cond
       ;; Section heading (## or ###): strip markdown and emoji for clean label
       ((string-match "^##+ +\\(.+\\)" line)
        (setq section (replace-regexp-in-string
                       "[[:nonascii:]] *" "" (match-string 1 line))))
       ;; Data row: not a separator (|---|)
       ((string-match-p "^|[^-]" line)
        (push (format "[%s] %s" (string-trim section) line) candidates))))
    (let* ((rows (nreverse candidates))
           (result (let ((completion-styles '(substring))
                         (completion-ignore-case t))
                     (completing-read "Keybinding: " rows nil t))))
      (message "%s" result))))

;;; Daily Inspiration Video
;; C-c i → stream a random short video (<5 min) in mpv
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
(defun my/watch-random-video (&optional arg)
  "Pick a random short inspiring video via AI-generated query and stream in mpv.

With no prefix: Claude Haiku (via GitHub Copilot backend) generates the
search query based on your interests, then yt-dlp finds a video ≤5 min.

With C-u prefix: choose backend interactively —
  Claude Haiku  — most creative, default
  Copilot       — GPT-4o via GitHub Copilot
  Local         — no AI, use built-in theme list

Records today as watched to suppress daily reminders."
  (interactive "P")
  (let* ((backends `(("Copilot GPT-5-mini" . (,rsr/gptel-github-backend . gpt-5-mini))
                     ("Claude Haiku"       . (,rsr/gptel-github-backend . claude-haiku-4.5))
                     ("Copilot GPT-4o"    . (,rsr/gptel-github-backend . gpt-4o))
                     ("Local (no AI)"     . nil)))
         (choice   (if arg
                       (completing-read "AI backend: " (mapcar #'car backends) nil t)
                     "Copilot GPT-5-mini"))
         (backend-pair (cdr (assoc choice backends))))
    (if (null backend-pair)
        ;; Local fallback — no AI
        (progn
          (message "Finding inspiration (local)...")
          (my/video-launch-script nil))
      ;; AI path — let-bind backend/model, single non-streaming request
      (message "Asking %s for inspiration..." choice)
      (let ((gptel-backend (car backend-pair))
            (gptel-model   (cdr backend-pair)))
        (gptel-request my/video-ai-prompt
          :stream   nil
          :callback (lambda (response info)
                      (if (stringp response)
                          (my/video-launch-script (string-trim response))
                        ;; AI failed — show status and fall back to built-in list
                        (message "AI failed (%s), using built-in theme..."
                                 (plist-get info :status))
                        (my/video-launch-script nil))))))))

(defun my/video-maybe-remind ()
  "Nudge if no inspiration video has been watched today."
  (unless (my/video-watched-today-p)
    (message "[Inspiration] Haven't watched today — C-c i to get inspired.")))

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

;; The NS build defined these in ns-win.el; the emacs-mac port does not, since
;; it assumes Cmd=meta. We remap Cmd to super in early-init.el, so recreate the
;; standard macOS chords here. Cmd-C matters most: M-w is unreachable because
;; the window manager binds Option-w.
(global-set-key (kbd "s-s")        #'save-buffer)
(global-set-key (kbd "s-c")        #'kill-ring-save)
(global-set-key (kbd "s-v")        #'yank)
(global-set-key (kbd "s-x")        #'kill-region)
(global-set-key (kbd "s-z")        #'undo)
(global-set-key (kbd "s-a")        #'mark-whole-buffer)

;; Zoom. One function for all four: `text-scale-adjust' reads the key that
;; invoked it to decide in/out/reset, and stays active for bare = / - repeats.
;; Inert on Linux — `x-super-modifier' is meta there, so nothing emits s-.
;; The portable equivalent is `C-x C-=', which works on every platform.
(global-set-key (kbd "s-=")        #'text-scale-adjust)  ;; Cmd+=       zoom in
(global-set-key (kbd "s-+")        #'text-scale-adjust)  ;; Cmd+Shift+= zoom in
(global-set-key (kbd "s--")        #'text-scale-adjust)  ;; Cmd+-       zoom out
(global-set-key (kbd "s-0")        #'text-scale-adjust)  ;; Cmd+0       reset

(global-set-key (kbd "s-F")        #'consult-ripgrep)  ;; Cmd+Shift+F — project-wide search
(global-set-key (kbd "M-g")        #'rgrep)
(global-set-key (kbd "s-l")        #'rsr/select-whole-line)
(global-set-key (kbd "s-/")        #'rsr/comment-or-uncomment)
(global-set-key (kbd "C-/")        #'rsr/comment-or-uncomment)
(global-set-key (kbd "s-k")        #'kill-whole-line)
(global-set-key (kbd "C-c l")      #'org-store-link)

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
(global-set-key (kbd "C-c L")      #'rsr/org-store-link-to-clipboard)

(global-set-key (kbd "C-c i")      #'my/watch-random-video)

;; Focus-timer: non-blocking bridge to the standalone ~/focus-timer web app
;; (Node server + browser/mpv dial + menu-bar SwiftBar plugin). Independent of
;; org-clock and Toggl — see ~/focus-timer/README.md.
;; C-c f: menu-bar only. C-u C-c f: also opens the full-screen dial window.
;; M-m f w: open the dial window on its own, any time, without touching the
;; running block (e.g. started menu-bar-only, now want the window too).
(when (file-exists-p "~/focus-timer/focus-timer.el")
  (load "~/focus-timer/focus-timer.el"))
(global-set-key (kbd "C-c f")      #'rsr/focus-start-at-point)

(bind-keys :map rsr/global-prefix-map
           ("t c" . calc)
           ("t d" . dictionary-search)
           ("f w" . rsr/focus-show-window))

(provide 'tools)
;;; tools.el ends here
