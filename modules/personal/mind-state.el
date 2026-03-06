;;; mind-state.el --- Random mind-state prompts for daily awareness -*- lexical-binding: t; -*-

;;; Code:

(require 'org-capture)
(require 'paths)

;;; --- Configuration ---

(defvar mind-state-target-prompts 10
  "Target number of mind-state prompts per day.")

(defvar mind-state-active-hours '(8 22)
  "Hours between which mind-state prompts are active (24-hour format).")

(defvar mind-state-min-interval 30
  "Minimum minutes between mind-state prompts.")

(defvar mind-state-max-interval 180
  "Maximum minutes between mind-state prompts.")

(defvar mind-state-idle-threshold 300
  "Minimum seconds of Emacs activity required before showing prompt.")

(defvar mind-state-options
  '("Focused" "Distracted" "Confused" "Stressed"
    "Tired" "Sleepy" "Bored" "Calm"
    "Anxious" "Creative" "Inspired" "Content"
    "Hungry" "Overwhelmed" "Motivated" "Procrastinating"
    "Clear" "Curious" "Restless" "Energetic")
  "Predefined mind-state options.")

;;; --- Internal State ---

(defvar mind-state-timer nil)
(defvar mind-state-prompts-today 0)
(defvar mind-state-today-date nil)

;;; --- Internal Helpers ---

(defun mind-state--current-date-string ()
  "Return today's date as YYYY-MM-DD string."
  (format-time-string "%Y-%m-%d"))

(defun mind-state--reset-daily-counter ()
  "Reset prompt counter if the date has changed."
  (let ((today (mind-state--current-date-string)))
    (unless (string= today mind-state-today-date)
      (setq mind-state-today-date today
            mind-state-prompts-today 0))))

(defun mind-state--within-active-hours-p ()
  "Return t if current time is within active hours."
  (let ((hour (string-to-number (format-time-string "%H")))
        (start (car mind-state-active-hours))
        (end (cadr mind-state-active-hours)))
    (and (>= hour start) (< hour end))))

(defun mind-state--user-is-active-p ()
  "Return t if user has been active recently (not idle)."
  (let ((idle (current-idle-time)))
    (if idle
        (< (float-time idle) mind-state-idle-threshold)
      t)))

(defun mind-state--should-prompt-p ()
  "Return t if conditions are met to show a prompt."
  (and (mind-state--within-active-hours-p)
       (mind-state--user-is-active-p)
       (< mind-state-prompts-today mind-state-target-prompts)))

(defun mind-state--calculate-next-interval ()
  "Calculate random interval in seconds until next prompt."
  (let* ((remaining (max 1 (- mind-state-target-prompts mind-state-prompts-today)))
         (hours-left (max 1 (- (cadr mind-state-active-hours)
                               (string-to-number (format-time-string "%H")))))
         (avg (/ (* hours-left 60) remaining))
         (lo (max mind-state-min-interval (/ avg 2)))
         (hi (max lo (min mind-state-max-interval (* avg 2)))))
    (* (+ lo (random (1+ (- hi lo)))) 60)))

(defun mind-state--ensure-daily-heading ()
  "Create today's top-level heading in journal if it doesn't exist."
  (goto-char (point-min))
  (let* ((heading (format-time-string "%Y-%m-%d %A"))
         (regex (format "^\\* %s$" heading)))
    (unless (re-search-forward regex nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "\n* %s\n" heading)))
    (end-of-line)))

;;; --- Core Prompt ---

(defun mind-state--prompt-user ()
  "Prompt user for current mind-state and log it to journal."
  (when (mind-state--should-prompt-p)
    (let* ((state (completing-read "Mind-state: " mind-state-options nil nil))
           (state (if (string-empty-p state)
                      (read-string "Describe your state: ")
                    state))
           (notes (read-string "Notes (optional): "))
           (entry (concat "Mind-State: " state
                          (unless (string-empty-p notes)
                            (concat " | " notes)))))
      (with-current-buffer (find-file-noselect my/journal-file)
        (save-excursion
          (mind-state--ensure-daily-heading)
          (goto-char (point-min))
          (when (re-search-forward (format-time-string "^\\* %Y-%m-%d %A$") nil t)
            (forward-line 1)
            (insert (format "** %s:\n:PROPERTIES:\n:PROJECT: Habits\n:END:\n- %s\n"
                            (format-time-string "%I:%M %p")
                            entry))))
        (save-buffer))
      (setq mind-state-prompts-today (1+ mind-state-prompts-today))
      (message "Logged: %s (%d/%d today)"
               state mind-state-prompts-today mind-state-target-prompts)))
  (mind-state--schedule-next-prompt))

;;; --- Timer Management ---

(defun mind-state--schedule-next-prompt ()
  "Schedule the next mind-state prompt."
  (when mind-state-timer
    (cancel-timer mind-state-timer))
  (when (< mind-state-prompts-today mind-state-target-prompts)
    (let ((interval (mind-state--calculate-next-interval)))
      (setq mind-state-timer
            (run-at-time interval nil #'mind-state--prompt-user))
      (message "Next mind-state check in ~%d minutes" (/ interval 60)))))

;;; --- Public Commands ---

(defun mind-state-start-tracking ()
  "Start mind-state tracking for today."
  (interactive)
  (mind-state--reset-daily-counter)
  (mind-state--schedule-next-prompt)
  (message "Mind-state tracking started. Target: %d prompts today."
           mind-state-target-prompts))

(defun mind-state-stop-tracking ()
  "Stop mind-state tracking."
  (interactive)
  (when mind-state-timer
    (cancel-timer mind-state-timer)
    (setq mind-state-timer nil))
  (message "Mind-state tracking stopped."))

(defun mind-state-manual-log ()
  "Manually trigger a mind-state log entry, bypassing schedule guards."
  (interactive)
  (mind-state--reset-daily-counter)
  (let* ((state (completing-read "Mind-state: " mind-state-options nil nil))
         (state (if (string-empty-p state)
                    (read-string "Describe your state: ")
                  state))
         (notes (read-string "Notes (optional): "))
         (entry (concat "Mind-State: " state
                        (unless (string-empty-p notes)
                          (concat " | " notes)))))
    (with-current-buffer (find-file-noselect my/journal-file)
      (save-excursion
        (mind-state--ensure-daily-heading)
        (goto-char (point-min))
        (when (re-search-forward (format-time-string "^\\* %Y-%m-%d %A$") nil t)
          (forward-line 1)
          (insert (format "** %s:\n:PROPERTIES:\n:PROJECT: Habits\n:END:\n- %s\n"
                          (format-time-string "%I:%M %p")
                          entry))))
      (save-buffer))
    (setq mind-state-prompts-today (1+ mind-state-prompts-today))
    (message "Logged: %s (%d/%d today)"
             state mind-state-prompts-today mind-state-target-prompts)))

(defun mind-state-status ()
  "Show current mind-state tracking status."
  (interactive)
  (mind-state--reset-daily-counter)
  (message "Tracking: %s | Today: %d/%d | Active hours: %02d:00-%02d:00"
           (if mind-state-timer "ACTIVE" "INACTIVE")
           mind-state-prompts-today mind-state-target-prompts
           (car mind-state-active-hours) (cadr mind-state-active-hours)))

(defun mind-state-configure ()
  "Interactively configure mind-state tracking settings."
  (interactive)
  (setq mind-state-target-prompts
        (read-number "Target prompts per day: " mind-state-target-prompts))
  (setq mind-state-active-hours
        (list (read-number "Start hour (24-hour): " (car mind-state-active-hours))
              (read-number "End hour (24-hour): " (cadr mind-state-active-hours))))
  (setq mind-state-min-interval
        (read-number "Minimum interval (minutes): " mind-state-min-interval))
  (setq mind-state-max-interval
        (read-number "Maximum interval (minutes): " mind-state-max-interval))
  (message "Configuration updated. Restart tracking for changes to take effect."))

;;; --- Auto-start ---

(defun mind-state--auto-start ()
  "Auto-start tracking if within active hours."
  (when (mind-state--within-active-hours-p)
    (mind-state-start-tracking)))

(add-hook 'emacs-startup-hook #'mind-state--auto-start)

;;; Keybindings
(define-prefix-command 'mind-state-map)
(global-set-key (kbd "C-c s")   'mind-state-map)
(define-key mind-state-map (kbd "s") #'mind-state-start-tracking)
(define-key mind-state-map (kbd "q") #'mind-state-stop-tracking)
(define-key mind-state-map (kbd "l") #'mind-state-manual-log)
(define-key mind-state-map (kbd "?") #'mind-state-status)
(define-key mind-state-map (kbd "c") #'mind-state-configure)

(provide 'mind-state)
;;; mind-state.el ends here
