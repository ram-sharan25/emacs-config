;;; org-config.el --- Org-mode configuration -*- lexical-binding: t; -*-

;;; Code:

(require 'paths)
(require 'org-protocol)
(require 'org-tempo)

;;; Link Abbreviations
;; [[data:image.png]] expands to my/data-dir/image.png
(add-to-list 'org-link-abbrev-alist (cons "data" (concat my/data-dir "%s")))

;; imap-message: links — open Thunderbird email links from org files
(org-link-set-parameters "imap-message"
  :follow (lambda (path)
            (browse-url (concat "imap-message:" path))))

;;; Org Core Settings

(use-package org
  :config
  (setq org-html-head-include-default-style nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-startup-indented t)          ;; align content under heading text
  (setq org-startup-folded 'content)     ;; show headlines, hide body/drawers
  (setq org-hide-drawer-startup t)       ;; collapse all drawers on open
  (setq org-preview-latex-image-directory "/tmp/ltximg/")
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-latex-toc-command "\\tableofcontents \\clearpage")
  (plist-put org-format-latex-options :background "Transparent")

  ;; Scale LaTeX preview fragments when text-scale changes
  (defun rsr/adjust-latex-previews-scale ()
    "Resize latex preview overlays to match current text scale."
    (pcase major-mode
      ('latex-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (when (eq (overlay-get ov 'category) 'preview-overlay)
           (rsr/latex-preview--resize-fragment ov))))
      ('org-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (when (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
           (rsr/latex-preview--resize-fragment ov))))))

  (defun rsr/latex-preview--resize-fragment (ov)
    "Resize a single latex preview overlay OV to match text scale."
    (overlay-put ov 'display
                 (cons 'image
                       (plist-put (cdr (overlay-get ov 'display))
                                  :scale (* 2
                                            (/ (frame-char-height) 12)
                                            (expt text-scale-mode-step
                                                  text-scale-mode-amount))))))

  (add-hook 'text-scale-mode-hook #'rsr/adjust-latex-previews-scale)
  (advice-add 'org-latex-preview :after #'rsr/adjust-latex-previews-scale))

;;; THOUGHTS Drawer Highlighting
;; Visually distinguishes :THOUGHTS: drawers with a subtle background.
;; Tags inside (QUESTION:, HYPOTHESIS:, ANALYSIS:) get distinct colors.
;; Uses overlays applied after a 0.5s idle pause — no cost during typing.

(defface my/thought-face
  '((t (:background "#4a5750" :extend t)))
  "Background face for :THOUGHTS: drawer content.")

(defface my/thought-tag-q-face
  '((t (:foreground "#DFAF8F" :weight bold :background "#4a5750" :extend t)))
  "Face for QUESTION: tags inside :THOUGHTS: drawers.")

(defface my/thought-tag-h-face
  '((t (:foreground "#94BFF3" :weight bold :background "#4a5750" :extend t)))
  "Face for HYPOTHESIS: tags inside :THOUGHTS: drawers.")

(defface my/thought-tag-a-face
  '((t (:foreground "#F4F4D0" :weight bold :background "#4a5750" :extend t)))
  "Face for ANALYSIS:/ANSWER: tags inside :THOUGHTS: drawers.")

(defvar-local my/thought-drawer-overlays nil
  "Overlays marking :THOUGHTS: drawer backgrounds in this buffer.")

(defvar-local my/thought-drawer-timer nil
  "Idle timer for deferred :THOUGHTS: drawer overlay updates.")

(defun my/thought-drawer-clear-overlays ()
  "Remove all :THOUGHTS: drawer overlays from current buffer."
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
  "Schedule a deferred overlay update, cancelling any pending timer."
  (when my/thought-drawer-timer
    (cancel-timer my/thought-drawer-timer))
  (setq my/thought-drawer-timer
        (run-with-idle-timer 0.5 nil #'my/thought-drawer-apply-overlays)))

(defun my/activate-thought-highlighting ()
  "Enable :THOUGHTS: drawer highlighting with overlays and font-lock tags."
  (add-hook 'after-change-functions #'my/thought-drawer-schedule-update nil t)
  (my/thought-drawer-apply-overlays)
  (font-lock-add-keywords nil
    '(("\\<\\(QUESTION\\|Q\\):.*$"         (0 'my/thought-tag-q-face t))
      ("\\<\\(HYPOTHESIS\\|H\\):.*$"       (0 'my/thought-tag-h-face t))
      ("\\<\\(ANALYSIS\\|ANSWER\\|A\\):.*$" (0 'my/thought-tag-a-face t)))
    'append))

(add-hook 'org-mode-hook #'my/activate-thought-highlighting)

;;; Custom Timer
;; Reads :TIMER_MINUTES: and :NOTIFY_BEFORE_MINUTES: from the heading.
;; Auto-starts on clock-in, cancels on clock-out.
;; Plays alarm sound (afplay on macOS) or falls back to beep.

(defcustom my/timer-sound-file "/Users/rrimal/.emacs.d/data/alarm_sound.mp3"
  "Sound file to play for timer notifications. nil uses system beep."
  :type '(choice (const :tag "None" nil) (file :tag "Sound File"))
  :group 'org)

(defvar my/custom-timer-notification-object nil
  "Timer object for the pre-notification warning.")

(defun my/org-play-notification-sound ()
  "Play the notification sound via afplay (macOS) or fall back to beep."
  (let ((sound-file (and my/timer-sound-file (expand-file-name my/timer-sound-file))))
    (if (and sound-file (file-exists-p sound-file))
        (let ((proc (start-process "org-timer-sound" nil "afplay" sound-file)))
          (set-process-sentinel proc
                                (lambda (p e)
                                  (unless (eq 0 (process-exit-status p))
                                    (message "Sound process failed: %s" e)))))
      (beep))))

(defun my/org-start-custom-timer ()
  "Start a timer from :TIMER_MINUTES: with optional :NOTIFY_BEFORE_MINUTES: warning."
  (interactive)
  (when my/custom-timer-notification-object
    (cancel-timer my/custom-timer-notification-object)
    (setq my/custom-timer-notification-object nil))
  (let* ((minutes (string-to-number (or (org-entry-get (point) "TIMER_MINUTES") "0")))
         (notify-mins (string-to-number (or (org-entry-get (point) "NOTIFY_BEFORE_MINUTES") "0"))))
    (if (<= minutes 0)
        (message "No :TIMER_MINUTES: property found or invalid.")
      (org-timer-set-timer (format "%d" minutes))
      (when (and (> notify-mins 0) (< notify-mins minutes))
        (setq my/custom-timer-notification-object
              (run-at-time (format "%d sec" (* (- minutes notify-mins) 60)) nil
                           (lambda (rem)
                             (my/org-play-notification-sound)
                             (message "Timer: %d minutes remaining." rem))
                           notify-mins))))))

(defun my/org-timer-on-clock-in ()
  "Auto-start custom timer on clock-in if :TIMER_MINUTES: is set."
  (unless (bound-and-true-p my/mobile-sync-in-progress)
    (when (org-entry-get (point) "TIMER_MINUTES")
      (my/org-start-custom-timer))))

(defun my/org-cancel-timer-on-clock-out ()
  "Cancel the custom timer and notification on clock-out."
  (ignore-errors (org-timer-stop))
  (when my/custom-timer-notification-object
    (cancel-timer my/custom-timer-notification-object)
    (setq my/custom-timer-notification-object nil)))

(add-hook 'org-clock-in-hook  #'my/org-timer-on-clock-in)
(add-hook 'org-clock-out-hook #'my/org-cancel-timer-on-clock-out)

;;; Keybindings

;; C-c . / C-c ! always open date+time picker — RET inserts current date+time
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c .") (lambda ()
    (interactive)
    (org-time-stamp '(4))))
  (define-key org-mode-map (kbd "C-c !") (lambda ()
    (interactive)
    (org-time-stamp-inactive '(4)))))

(provide 'org-config)
;;; org-config.el ends here
