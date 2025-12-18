(setq org-html-head-include-default-style nil)

(require 'paths)
(require 'org-protocol)
(require 'org-tempo)

;; Link Abbreviations
;; Usage: [[data:image.png]] -> expands to my/data-dir/image.png
(add-to-list 'org-link-abbrev-alist (cons "data" (concat my/data-dir "%s")))

;; Usage: [[dsa_lec:video.mp4]] -> expands to my/dsa-lectures/video.mp4
(add-to-list 'org-link-abbrev-alist (cons "dsa_lec" (concat my/dsa-lectures "%s")))

(org-link-set-parameters "imap-message"
  :follow (lambda (path)
            (browse-url (concat "imap-message:" path))))


(defun bp/org-publish--add-setupfile (&rest args)
  (goto-char (point-min))
  (search-forward "#+title")
  (beginning-of-line)
  (insert "#+setupfile: /Users/rrimal/.emacs.d/modules/git-modules/src/comfy_inline/comfy_inline.theme\n"))

;; (use-package ox
;;   :defer t
;;   :config
;;   (add-hook 'org-export-before-processing-functions #'bp/org-publish--add-setupfile))


(use-package org
  :config
  (setq org-fontify-quote-and-verse-blocks t) ;; Enable special highlighting for quote blocks
  (setq org-preview-latex-image-directory "/tmp/ltximg/")
  (setq org-preview-latex-default-process 'dvisvgm)
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
  (defadvice org-latex-preview (after bp/org-latex-preview--adjust-scale activate)
    (bp/adjust-latex-previews-scale)))

;; -------------------------------------------------------------------------
;; Custom Highlighting for :THOUGHTS: Drawers
;; -------------------------------------------------------------------------

(defface my/thought-face
  '((t (:foreground "#DCDCCC" :background "#4a5750" :extend t)))
  "Face for THOUGHTS drawer background.")

(defface my/thought-tag-q-face
  '((t (:foreground "#DFAF8F" :weight bold :background "#4a5750" :extend t))) ;; Zenburn Orange (Curiosity)
  "Face for QUESTION tags.")

(defface my/thought-tag-h-face
  '((t (:foreground "#94BFF3" :weight bold :background "#4a5750" :extend t))) ;; Zenburn Blue (Theory)
  "Face for HYPOTHESIS tags.")

(defface my/thought-tag-a-face
  '((t (:foreground "#F4F4D0" :weight bold :background "#4a5750" :extend t))) ;; Zenburn Yellowish Cream (Fact)
  "Face for ANALYSIS tags.")

(defun my/thought-drawer-extend-region ()
  "Extend region to include the full drawer for background highlighting."
  (save-excursion
    (let ((changed nil))
      (goto-char font-lock-beg)
      (when (re-search-backward "^[ \t]*:THOUGHTS:" nil t)
        (setq font-lock-beg (match-beginning 0))
        (setq changed t))
      (goto-char font-lock-end)
      (when (re-search-forward "^[ \t]*:END:" nil t)
        (setq font-lock-end (match-end 0))
        (setq changed t))
      changed)))

(defun my/match-thought-drawer (limit)
  "Search for :THOUGHTS: drawer content."
  (let ((res nil))
    (while (and (not res) (re-search-forward "^[ \t]*:THOUGHTS:" limit t))
      (let ((start (line-beginning-position 2))
            (end (save-excursion
                   (if (re-search-forward "^[ \t]*:END:" nil t)
                       (match-beginning 0)
                     (point-max)))))
        (when (< start end)
          (put-text-property start end 'font-lock-multiline t)
          (set-match-data (list start end))
          (goto-char end)
          (setq res t))))
    res))

(defun my/activate-thought-highlighting ()
  "Add custom font-lock keywords for THOUGHTS drawers."
  (add-hook 'font-lock-extend-region-functions #'my/thought-drawer-extend-region nil t)

  (font-lock-add-keywords nil
    '((my/match-thought-drawer 0 'my/thought-face t)
      ("\\<\\(QUESTION\\|Q\\):.*$" (0 'my/thought-tag-q-face t))
      ("\\<\\(HYPOTHESIS\\|H\\):.*$" (0 'my/thought-tag-h-face t))
      ("\\<\\(ANALYSIS\\|ANSWER\\|A\\):.*$" (0 'my/thought-tag-a-face t)))
    'append))

(add-hook 'org-mode-hook #'my/activate-thought-highlighting)
(add-hook 'org-mode-hook #'org-bullets-mode)

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
                 (file :tag "Sound File")))

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
  "Automatically start custom timer if :TIMER_MINUTES: property exists."
  (when (org-entry-get (point) "TIMER_MINUTES")
    (my/org-start-custom-timer)))

(defun my/org-cancel-timer-on-clock-out ()
  "Cancel the custom timer and notification when clocking out."
  (ignore-errors (org-timer-stop)) ;; Prevent error if no timer is running
  (when my/custom-timer-notification-object
    (cancel-timer my/custom-timer-notification-object)
    (setq my/custom-timer-notification-object nil)))

(add-hook 'org-clock-in-hook #'my/org-timer-on-clock-in)
(add-hook 'org-clock-out-hook #'my/org-cancel-timer-on-clock-out)
