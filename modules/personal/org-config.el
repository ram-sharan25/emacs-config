(setq org-html-head-include-default-style nil)

(require 'paths)
(require 'org-protocol)
(require 'org-tempo)

;; Link Abbreviations
;; Usage: [[data:image.png]] -> expands to my/data-dir/image.png
(add-to-list 'org-link-abbrev-alist (cons "data" (concat my/data-dir "%s")))

(org-link-set-parameters "imap-message"
  :follow (lambda (path)
            (browse-url (concat "imap-message:" path))))


(use-package org
  :config
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-preview-latex-image-directory "/tmp/ltximg/")
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-latex-toc-command "\\tableofcontents \\clearpage")
  (plist-put org-format-latex-options :background "Transparent")

  (defun bp/adjust-latex-previews-scale ()
    "Adjust the size of latex preview fragments when changing the
buffer's text scale."
    (pcase major-mode
      ('latex-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'category)
                 'preview-overlay)
             (bp/latex-preview--resize-fragment ov))))
      ('org-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'org-overlay-type)
                 'org-latex-overlay)
             (bp/latex-preview--resize-fragment ov))))))

  (defun bp/latex-preview--resize-fragment (ov)
    (overlay-put
     ov 'display
     (cons 'image
           (plist-put
            (cdr (overlay-get ov 'display))
            :scale (* 2 (/ (frame-char-height) 12) (expt text-scale-mode-step text-scale-mode-amount))))))

  (add-hook 'text-scale-mode-hook #'bp/adjust-latex-previews-scale)
  (advice-add 'org-latex-preview :after #'bp/adjust-latex-previews-scale))

;; -------------------------------------------------------------------------
;; Custom Highlighting for :THOUGHTS: Drawers
;; -------------------------------------------------------------------------

;; Background-only face for the overlay — no foreground, so font-lock tag
;; colors (below) can show through via face attribute merging.
(defface my/thought-face
  '((t (:background "#4a5750" :extend t)))
  "Background face for THOUGHTS drawer (applied via overlay).")

(defface my/thought-tag-q-face
  '((t (:foreground "#DFAF8F" :weight bold :background "#4a5750" :extend t))) ;; Zenburn Orange (Curiosity)
  "Face for QUESTION tags.")

(defface my/thought-tag-h-face
  '((t (:foreground "#94BFF3" :weight bold :background "#4a5750" :extend t))) ;; Zenburn Blue (Theory)
  "Face for HYPOTHESIS tags.")

(defface my/thought-tag-a-face
  '((t (:foreground "#F4F4D0" :weight bold :background "#4a5750" :extend t))) ;; Zenburn Yellowish Cream (Fact)
  "Face for ANALYSIS tags.")

;; Overlay-based highlighting: zero cost during active typing.
;; Overlays are applied after a 0.5s idle pause via after-change-functions.
(defvar-local my/thought-drawer-overlays nil
  "Overlays marking :THOUGHTS: drawer backgrounds in this buffer.")

(defvar-local my/thought-drawer-timer nil
  "Idle timer for deferred THOUGHTS drawer overlay updates.")

(defun my/thought-drawer-clear-overlays ()
  (mapc #'delete-overlay my/thought-drawer-overlays)
  (setq my/thought-drawer-overlays nil))

(defun my/thought-drawer-apply-overlays ()
  "Scan buffer and place background overlays on :THOUGHTS: drawer content."
  (when (derived-mode-p 'org-mode)
    (my/thought-drawer-clear-overlays)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:THOUGHTS:" nil t)
        (let ((start (line-beginning-position 2))
              (end (save-excursion
                     (if (re-search-forward "^[ \t]*:END:" nil t)
                         (match-beginning 0)
                       (point-max)))))
          (when (< start end)
            (let ((ov (make-overlay start end)))
              (overlay-put ov 'face 'my/thought-face)
              (overlay-put ov 'my/thought-drawer t)
              (push ov my/thought-drawer-overlays))))))))

(defun my/thought-drawer-schedule-update (&rest _)
  "Schedule a deferred overlay update, cancelling any pending one."
  (when my/thought-drawer-timer
    (cancel-timer my/thought-drawer-timer))
  (setq my/thought-drawer-timer
        (run-with-idle-timer 0.5 nil #'my/thought-drawer-apply-overlays)))

(defun my/activate-thought-highlighting ()
  "Set up THOUGHTS drawer highlighting with overlays and font-lock for tags."
  ;; Overlays update after user pauses — no cost during active typing.
  (add-hook 'after-change-functions #'my/thought-drawer-schedule-update nil t)
  (my/thought-drawer-apply-overlays)
  ;; Single-line tag patterns: no extend-region machinery needed.
  (font-lock-add-keywords nil
    '(("\\<\\(QUESTION\\|Q\\):.*$" (0 'my/thought-tag-q-face t))
      ("\\<\\(HYPOTHESIS\\|H\\):.*$" (0 'my/thought-tag-h-face t))
      ("\\<\\(ANALYSIS\\|ANSWER\\|A\\):.*$" (0 'my/thought-tag-a-face t)))
    'append))

(add-hook 'org-mode-hook #'my/activate-thought-highlighting)
(add-hook 'org-mode-hook #'org-bullets-mode)

;; Visual indentation: content aligns under heading text, stars hidden
(setq org-startup-indented t)

;; Clean Emacs Way: Use standard variables to control visibility
(setq org-startup-folded 'content) ;; Show headlines, hide content/drawers
(setq org-hide-drawer-startup t)   ;; Explicitly collapse all drawers

;; -------------------------------------------------------------------------
;; Custom Timer for Grounding Break
;; -------------------------------------------------------------------------

(defcustom my/timer-sound-file "/Users/rrimal/.emacs.d/data/alarm_sound.mp3"
  "Path to a sound file to play for timer notifications.
If nil, a system beep is used."
  :type '(choice (const :tag "None" nil)
                 (file :tag "Sound File"))
  :group 'org)

(defvar my/custom-timer-notification-object nil
  "Stores the timer object for the pre-notification warning.")

(defun my/org-play-notification-sound ()
  "Play the notification sound using afplay (macOS) or beep."
  (let ((sound-file (and my/timer-sound-file (expand-file-name my/timer-sound-file))))
    (if (and sound-file (file-exists-p sound-file))
        (progn
          (message "Playing sound: %s" sound-file)
          (let ((proc (start-process "org-timer-sound" nil "afplay" sound-file)))
            (set-process-sentinel proc
                                  (lambda (p e)
                                    (when (not (eq 0 (process-exit-status p)))
                                      (message "Sound process failed: %s" e))))))
      (message "Sound file not found: %s. Beeping." sound-file)
      (beep))))

(defun my/org-start-custom-timer ()
  "Start a timer based on :TIMER_MINUTES: property with a pre-notification.
Reads :TIMER_MINUTES: and :NOTIFY_BEFORE_MINUTES: from the current heading."
  (interactive)
  ;; Cancel any existing notification timer first
  (when my/custom-timer-notification-object
    (cancel-timer my/custom-timer-notification-object)
    (setq my/custom-timer-notification-object nil))

  (let* ((minutes-str (org-entry-get (point) "TIMER_MINUTES"))
         (notify-str (org-entry-get (point) "NOTIFY_BEFORE_MINUTES"))
         (minutes (if minutes-str (string-to-number minutes-str) 0))
         (notify-mins (if notify-str (string-to-number notify-str) 0)))

    (if (or (not minutes) (<= minutes 0))
        (message "No :TIMER_MINUTES: property found or invalid.")
      ;; 1. Start the main Org timer
      (org-timer-set-timer (format "%d" minutes))

      ;; 2. Schedule the pre-notification if configured
      (when (and notify-mins (> notify-mins 0) (< notify-mins minutes))
        (let ((notify-delay-sec (* (- minutes notify-mins) 60)))
          (setq my/custom-timer-notification-object
                (run-at-time (format "%d sec" notify-delay-sec) nil
                             (lambda (rem-mins)
                               (my/org-play-notification-sound)
                               (message "⚠️ Time is almost up! %d minutes remaining." rem-mins))
                             notify-mins)))))))

(defun my/org-timer-on-clock-in ()
  "Automatically start custom timer if :TIMER_MINUTES: property exists.
Skips execution if `my/mobile-sync-in-progress' is non-nil."
  (unless (bound-and-true-p my/mobile-sync-in-progress)
    (when (org-entry-get (point) "TIMER_MINUTES")
      (my/org-start-custom-timer))))

(defun my/org-cancel-timer-on-clock-out ()
  "Cancel the custom timer and notification when clocking out."
  (ignore-errors (org-timer-stop)) ;; Prevent error if no timer is running
  (when my/custom-timer-notification-object
    (cancel-timer my/custom-timer-notification-object)
    (setq my/custom-timer-notification-object nil)))

(add-hook 'org-clock-in-hook #'my/org-timer-on-clock-in)
(add-hook 'org-clock-out-hook #'my/org-cancel-timer-on-clock-out)
